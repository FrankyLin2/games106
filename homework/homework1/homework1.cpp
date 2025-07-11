/*
* Vulkan Example - glTF scene loading and rendering
*
* Copyright (C) 2020-2022 by Sascha Willems - www.saschawillems.de
*
* This code is licensed under the MIT license (MIT) (http://opensource.org/licenses/MIT)
*/

/*
 * Shows how to load and display a simple scene from a glTF file
 * Note that this isn't a complete glTF loader and only basic functions are shown here
 * This means no complex materials, no animations, no skins, etc.
 * For details on how glTF 2.0 works, see the official spec at https://github.com/KhronosGroup/glTF/tree/master/specification/2.0
 *
 * Other samples will load models using a dedicated model loader with more features (see base/VulkanglTFModel.hpp)
 *
 * If you are looking for a complete glTF implementation, check out https://github.com/SaschaWillems/Vulkan-glTF-PBR/
 */

#define TINYGLTF_IMPLEMENTATION
#define STB_IMAGE_IMPLEMENTATION
#define TINYGLTF_NO_STB_IMAGE_WRITE
#ifdef VK_USE_PLATFORM_ANDROID_KHR
#define TINYGLTF_ANDROID_LOAD_FROM_ASSETS
#endif
#include "tiny_gltf.h"

#include "vulkanexamplebase.h"

#define ENABLE_VALIDATION false

// Contains everything required to render a glTF model in Vulkan
// This class is heavily simplified (compared to glTF's feature set) but retains the basic glTF structure
class VulkanglTFModel
{
public:
	// The class requires some Vulkan objects so it can create it's own resources
	vks::VulkanDevice* vulkanDevice;
	VkQueue copyQueue;

	// The vertex layout for the samples' model
	struct Vertex {
		glm::vec3 pos;
		glm::vec3 normal;
		glm::vec2 uv;
		glm::vec3 color;
		uint32_t nodeIndex;  // 新增: 节点索引
		glm::vec4 jointIndices;
		glm::vec4 jointWeights;
	};

	// Single vertex buffer for all primitives
	struct {
		VkBuffer buffer;
		VkDeviceMemory memory;
	} vertices;

	// Single index buffer for all primitives
	struct {
		int count;
		VkBuffer buffer;
		VkDeviceMemory memory;
	} indices;

	// The following structures roughly represent the glTF scene structure
	// To keep things simple, they only contain those properties that are required for this sample
	struct Node;

	// A primitive contains the data for a single draw call
	struct Primitive {
		uint32_t firstIndex;
		uint32_t indexCount;
		int32_t materialIndex;
	};

	// Contains the node's (optional) geometry and can be made up of an arbitrary number of primitives
	struct Mesh {
		std::vector<Primitive> primitives;
	};

	// A node represents an object in the glTF scene graph
	struct Node {
		Node* parent;
		uint32_t index;
		std::vector<Node*> children;
		Mesh mesh;
		glm::mat4 matrix;
		int32_t skin = -1;
		glm::vec3 translation = glm::vec3(0.0f);
		glm::quat rotation = glm::quat(1.0f, 0.0f, 0.0f, 0.0f);
		glm::vec3 scale = glm::vec3(1.0f);
		glm::mat4 getLocalMatrix() {
			// glTF规范：节点要么有matrix，要么有TRS，不应该同时应用
			// 如果matrix不是单位矩阵，则使用matrix；否则使用TRS计算
			if (matrix != glm::mat4(1.0f)) {
				return matrix;
			} else {
				return glm::translate(glm::mat4(1.0f), translation) * glm::mat4(rotation) * glm::scale(glm::mat4(1.0f), scale);
			}
		}
		~Node() {
			for (auto& child : children) {
				delete child;
			}
		}
	};

	/*
		Skin structure
	*/
	struct Skin
	{
		std::string            name;
		Node *                 skeletonRoot = nullptr;
		// 逆绑定矩阵，用于将模型空间中的顶点坐标转换到各个骨骼空间
		std::vector<glm::mat4> inverseBindMatrices;
		// 关节，其实是node
		std::vector<Node *>    joints;
		// shader storage buffer object，用于存储逆绑定矩阵，供GPU读取
		vks::Buffer            ssbo;
		// 描述符集，用于将逆绑定矩阵传递给着色器
		VkDescriptorSet        descriptorSet;
	};

	/*
		Animation related structures
	*/

	struct AnimationSampler
	{
		std::string            interpolation;
		// 输入时间
		std::vector<float>     inputs;
		// 输出对应时间点的关节属性，比如位置、旋转、缩放
		std::vector<glm::vec4> outputsVec4;
	};

	struct AnimationChannel
	{
		// 关节路径，比如"translation"、"rotation"、"scale"
		std::string path;
		// 关节
		Node *      node;
		// 采样器索引
		uint32_t    samplerIndex;
	};

	struct Animation
	{
		// 动画名称
		std::string                   name;
		// 采样器
		std::vector<AnimationSampler> samplers;
		// 通道
		std::vector<AnimationChannel> channels;
		// 开始时间
		float                         start       = std::numeric_limits<float>::max();
		float                         end         = std::numeric_limits<float>::min();
		float                         currentTime = 0.0f;
	};

	struct AnimationTransform
	{
		// 动画矩阵	
		std::vector<glm::mat4> animMatrices;
		vks::Buffer ssbo;
		VkDescriptorSet descriptorSet;
	}animationTransforms;
	
	// A glTF material stores information in e.g. the texture that is attached to it and colors
	struct Material {
		glm::vec4 baseColorFactor = glm::vec4(1.0f);
		uint32_t baseColorTextureIndex = 0;
		uint32_t normalTextureIndex = 0;
		uint32_t metallicRoughnessTextureIndex = 0;
		uint32_t occlusionTextureIndex = 0;
		uint32_t emissiveTextureIndex = 0;
		glm::vec3 emissiveFactor = glm::vec3(0.0f);
		std::string alphaMode = "OPAQUE";
		float alphaCutoff = 0.5f;
		bool doubleSided = false;
		// We also store (and create) a descriptor set that's used to access this material's textures from the fragment shader
		VkDescriptorSet descriptorSet;
	};

	// Contains the texture for a single glTF image
	// Images may be reused by texture objects and are as such separated
	struct Image {
		vks::Texture2D texture;
	};

	// A glTF texture stores a reference to the image and a sampler
	// In this sample, we are only interested in the image
	struct Texture {
		int32_t imageIndex;
	};

	/*
		Model data
	*/
	std::vector<Image> images;
	std::vector<Texture> textures;
	std::vector<Material> materials;
	std::vector<Node*> nodes;
	std::vector<Skin> skins;
	std::vector<Animation> animations;
	uint32_t activeAnimation = 0;
	~VulkanglTFModel()
	{
		for (auto node : nodes) {
			delete node;
		}
		// Release all Vulkan resources allocated for the model
		vkDestroyBuffer(vulkanDevice->logicalDevice, vertices.buffer, nullptr);
		vkFreeMemory(vulkanDevice->logicalDevice, vertices.memory, nullptr);
		vkDestroyBuffer(vulkanDevice->logicalDevice, indices.buffer, nullptr);
		vkFreeMemory(vulkanDevice->logicalDevice, indices.memory, nullptr);
		for (Image image : images) {
			vkDestroyImageView(vulkanDevice->logicalDevice, image.texture.view, nullptr);
			vkDestroyImage(vulkanDevice->logicalDevice, image.texture.image, nullptr);
			vkDestroySampler(vulkanDevice->logicalDevice, image.texture.sampler, nullptr);
			vkFreeMemory(vulkanDevice->logicalDevice, image.texture.deviceMemory, nullptr);
		}
		
		for (Skin& skin : skins) {
			vkDestroyBuffer(vulkanDevice->logicalDevice, skin.ssbo.buffer, nullptr);
			vkFreeMemory(vulkanDevice->logicalDevice, skin.ssbo.memory, nullptr);
		}
		vkDestroyBuffer(vulkanDevice->logicalDevice, animationTransforms.ssbo.buffer, nullptr);
		vkFreeMemory(vulkanDevice->logicalDevice, animationTransforms.ssbo.memory, nullptr);
	}

	/*
		glTF loading functions

		The following functions take a glTF input model loaded via tinyglTF and convert all required data into our own structure
	*/

	void loadImages(tinygltf::Model& input)
	{
		// Images can be stored inside the glTF (which is the case for the sample model), so instead of directly
		// loading them from disk, we fetch them from the glTF loader and upload the buffers
		images.resize(input.images.size());
		for (size_t i = 0; i < input.images.size(); i++) {
			tinygltf::Image& glTFImage = input.images[i];
			// Get the image data from the glTF loader
			unsigned char* buffer = nullptr;
			VkDeviceSize bufferSize = 0;
			bool deleteBuffer = false;
			// We convert RGB-only images to RGBA, as most devices don't support RGB-formats in Vulkan
			if (glTFImage.component == 3) {
				bufferSize = glTFImage.width * glTFImage.height * 4;
				buffer = new unsigned char[bufferSize];
				unsigned char* rgba = buffer;
				unsigned char* rgb = &glTFImage.image[0];
				for (size_t i = 0; i < glTFImage.width * glTFImage.height; ++i) {
					memcpy(rgba, rgb, sizeof(unsigned char) * 3);
					rgba += 4;
					rgb += 3;
				}
				deleteBuffer = true;
			}
			else {
				buffer = &glTFImage.image[0];
				bufferSize = glTFImage.image.size();
			}
			// Load texture from image buffer
			images[i].texture.fromBuffer(buffer, bufferSize, VK_FORMAT_R8G8B8A8_UNORM, glTFImage.width, glTFImage.height, vulkanDevice, copyQueue);
			if (deleteBuffer) {
				delete[] buffer;
			}
		}
	}

	void loadTextures(tinygltf::Model& input)
	{
		textures.resize(input.textures.size());
		for (size_t i = 0; i < input.textures.size(); i++) {
			textures[i].imageIndex = input.textures[i].source;
		}
	}

	void loadMaterials(tinygltf::Model& input)
	{
		materials.resize(input.materials.size());
		for (size_t i = 0; i < input.materials.size(); i++) {
			// We get PBR material, load all the textures
			tinygltf::Material glTFMaterial = input.materials[i];
			// Get the base color factor
			if (glTFMaterial.values.find("baseColorFactor") != glTFMaterial.values.end()) {
				materials[i].baseColorFactor = glm::make_vec4(glTFMaterial.values["baseColorFactor"].ColorFactor().data());
			}
			// Get base color texture index
			if (glTFMaterial.values.find("baseColorTexture") != glTFMaterial.values.end()) {
				materials[i].baseColorTextureIndex = glTFMaterial.values["baseColorTexture"].TextureIndex();
			}
			if (glTFMaterial.values.find("normalTexture") != glTFMaterial.values.end()) {
				materials[i].normalTextureIndex = glTFMaterial.values["normalTexture"].TextureIndex();
			}
			if (glTFMaterial.values.find("metallicRoughnessTexture") != glTFMaterial.values.end()) {
				materials[i].metallicRoughnessTextureIndex = glTFMaterial.values["metallicRoughnessTexture"].TextureIndex();
			}
			if (glTFMaterial.values.find("occlusionTexture") != glTFMaterial.values.end()) {
				materials[i].occlusionTextureIndex = glTFMaterial.values["occlusionTexture"].TextureIndex();
			}
			if (glTFMaterial.values.find("emissiveTexture") != glTFMaterial.values.end()) {
				materials[i].emissiveTextureIndex = glTFMaterial.values["emissiveTexture"].TextureIndex();
			}
			if (glTFMaterial.values.find("alphaMode") != glTFMaterial.values.end()) {
				materials[i].alphaMode = glTFMaterial.values["alphaMode"].string_value;
			}
			if (glTFMaterial.values.find("alphaCutoff") != glTFMaterial.values.end()) {
				materials[i].alphaCutoff = glTFMaterial.values["alphaCutoff"].number_value;
			}
			if (glTFMaterial.values.find("doubleSided") != glTFMaterial.values.end()) {
				materials[i].doubleSided = glTFMaterial.values["doubleSided"].bool_value;
			}
			if (glTFMaterial.values.find("emissiveFactor") != glTFMaterial.values.end()) {
				materials[i].emissiveFactor = glm::make_vec3(glTFMaterial.values["emissiveFactor"].ColorFactor().data());
			}

		}
	}

	void loadNode(const tinygltf::Node& inputNode, const tinygltf::Model& input, VulkanglTFModel::Node* parent, uint32_t nodeIndex, std::vector<uint32_t>& indexBuffer, std::vector<VulkanglTFModel::Vertex>& vertexBuffer)
	{
		VulkanglTFModel::Node* node = new VulkanglTFModel::Node{};
		node->matrix = glm::mat4(1.0f);
		node->parent = parent;
		node->index = nodeIndex;
		// Get the local node matrix
		// It's either made up from translation, rotation, scale or a 4x4 matrix
		if (inputNode.translation.size() == 3)
		{
			node->translation = glm::make_vec3(inputNode.translation.data());
		}
		if (inputNode.rotation.size() == 4)
		{
			node->rotation = glm::make_quat(inputNode.rotation.data());
		}
		if (inputNode.scale.size() == 3)
		{
			node->scale = glm::make_vec3(inputNode.scale.data());
		}
		if (inputNode.matrix.size() == 16)
		{
			node->matrix = glm::make_mat4x4(inputNode.matrix.data());
		};

		// Load node's children
		if (inputNode.children.size() > 0) {
			for (size_t i = 0; i < inputNode.children.size(); i++) {
				loadNode(input.nodes[inputNode.children[i]], input , node, inputNode.children[i], indexBuffer, vertexBuffer);
			}
		}

		// If the node contains mesh data, we load vertices and indices from the buffers
		// In glTF this is done via accessors and buffer views
		if (inputNode.mesh > -1) {
			const tinygltf::Mesh mesh = input.meshes[inputNode.mesh];
			// Iterate through all primitives of this node's mesh
			for (size_t i = 0; i < mesh.primitives.size(); i++) {
				const tinygltf::Primitive& glTFPrimitive = mesh.primitives[i];
				uint32_t firstIndex = static_cast<uint32_t>(indexBuffer.size());
				uint32_t vertexStart = static_cast<uint32_t>(vertexBuffer.size());
				uint32_t indexCount = 0;
				// Vertices
				{
					const float* positionBuffer = nullptr;
					const float* normalsBuffer = nullptr;
					const float* texCoordsBuffer = nullptr;
					const uint16_t* jointIndicesBuffer = nullptr;
					const float* jointWeightsBuffer = nullptr;
					bool hasSkin = false;
					size_t vertexCount = 0;

					// Get buffer data for vertex positions
					if (glTFPrimitive.attributes.find("POSITION") != glTFPrimitive.attributes.end()) {
						const tinygltf::Accessor& accessor = input.accessors[glTFPrimitive.attributes.find("POSITION")->second];
						const tinygltf::BufferView& view = input.bufferViews[accessor.bufferView];
						positionBuffer = reinterpret_cast<const float*>(&(input.buffers[view.buffer].data[accessor.byteOffset + view.byteOffset]));
						vertexCount = accessor.count;
					}
					// Get buffer data for vertex normals
					if (glTFPrimitive.attributes.find("NORMAL") != glTFPrimitive.attributes.end()) {
						const tinygltf::Accessor& accessor = input.accessors[glTFPrimitive.attributes.find("NORMAL")->second];
						const tinygltf::BufferView& view = input.bufferViews[accessor.bufferView];
						normalsBuffer = reinterpret_cast<const float*>(&(input.buffers[view.buffer].data[accessor.byteOffset + view.byteOffset]));
					}
					// Get buffer data for vertex texture coordinates
					// glTF supports multiple sets, we only load the first one
					if (glTFPrimitive.attributes.find("TEXCOORD_0") != glTFPrimitive.attributes.end()) {
						const tinygltf::Accessor& accessor = input.accessors[glTFPrimitive.attributes.find("TEXCOORD_0")->second];
						const tinygltf::BufferView& view = input.bufferViews[accessor.bufferView];
						texCoordsBuffer = reinterpret_cast<const float*>(&(input.buffers[view.buffer].data[accessor.byteOffset + view.byteOffset]));
					}
					// Get buffer data for vertex joint indices
					if (glTFPrimitive.attributes.find("JOINTS_0") != glTFPrimitive.attributes.end()) {
						const tinygltf::Accessor& accessor = input.accessors[glTFPrimitive.attributes.find("JOINTS_0")->second];
						const tinygltf::BufferView& view = input.bufferViews[accessor.bufferView];
						jointIndicesBuffer = reinterpret_cast<const uint16_t*>(&(input.buffers[view.buffer].data[accessor.byteOffset + view.byteOffset]));
					}
					// Get buffer data for vertex joint weights
					if (glTFPrimitive.attributes.find("WEIGHTS_0") != glTFPrimitive.attributes.end()) {
						const tinygltf::Accessor& accessor = input.accessors[glTFPrimitive.attributes.find("WEIGHTS_0")->second];
						const tinygltf::BufferView& view = input.bufferViews[accessor.bufferView];
						jointWeightsBuffer = reinterpret_cast<const float*>(&(input.buffers[view.buffer].data[accessor.byteOffset + view.byteOffset]));
					}
					hasSkin = (jointIndicesBuffer && jointWeightsBuffer);

					// Append data to model's vertex buffer
					for (size_t v = 0; v < vertexCount; v++) {
						Vertex vert{};
						vert.pos = glm::vec4(glm::make_vec3(&positionBuffer[v * 3]), 1.0f);
						vert.normal = glm::normalize(glm::vec3(normalsBuffer ? glm::make_vec3(&normalsBuffer[v * 3]) : glm::vec3(0.0f)));
						vert.uv = texCoordsBuffer ? glm::make_vec2(&texCoordsBuffer[v * 2]) : glm::vec3(0.0f);
						vert.color = glm::vec3(1.0f);
						vert.nodeIndex = nodeIndex;  // 设置当前节点索引
						vert.jointIndices = hasSkin ? glm::vec4(glm::make_vec4(&jointIndicesBuffer[v * 4])) : glm::vec4(0.0f);
						vert.jointWeights = hasSkin ? glm::make_vec4(&jointWeightsBuffer[v * 4]) : glm::vec4(0.0f);
						vertexBuffer.push_back(vert);
					}
				}
				// Indices
				{
					const tinygltf::Accessor& accessor = input.accessors[glTFPrimitive.indices];
					const tinygltf::BufferView& bufferView = input.bufferViews[accessor.bufferView];
					const tinygltf::Buffer& buffer = input.buffers[bufferView.buffer];

					indexCount += static_cast<uint32_t>(accessor.count);

					// glTF supports different component types of indices
					switch (accessor.componentType) {
					case TINYGLTF_PARAMETER_TYPE_UNSIGNED_INT: {
						const uint32_t* buf = reinterpret_cast<const uint32_t*>(&buffer.data[accessor.byteOffset + bufferView.byteOffset]);
						for (size_t index = 0; index < accessor.count; index++) {
							indexBuffer.push_back(buf[index] + vertexStart);
						}
						break;
					}
					case TINYGLTF_PARAMETER_TYPE_UNSIGNED_SHORT: {
						const uint16_t* buf = reinterpret_cast<const uint16_t*>(&buffer.data[accessor.byteOffset + bufferView.byteOffset]);
						for (size_t index = 0; index < accessor.count; index++) {
							indexBuffer.push_back(buf[index] + vertexStart);
						}
						break;
					}
					case TINYGLTF_PARAMETER_TYPE_UNSIGNED_BYTE: {
						const uint8_t* buf = reinterpret_cast<const uint8_t*>(&buffer.data[accessor.byteOffset + bufferView.byteOffset]);
						for (size_t index = 0; index < accessor.count; index++) {
							indexBuffer.push_back(buf[index] + vertexStart);
						}
						break;
					}
					default:
						std::cerr << "Index component type " << accessor.componentType << " not supported!" << std::endl;
						return;
					}
				}
				Primitive primitive{};
				primitive.firstIndex = firstIndex;
				primitive.indexCount = indexCount;
				primitive.materialIndex = glTFPrimitive.material;
				node->mesh.primitives.push_back(primitive);
			}
		}

		if (parent) {
			parent->children.push_back(node);
		}
		else {
			nodes.push_back(node);
		}
	}


	/*
		glTF vertex skinning functions
	*/

	// POI: Traverse the node hierarchy to the top-most parent to get the local matrix of the given node
	glm::mat4 VulkanglTFModel::getNodeMatrix(VulkanglTFModel::Node *node)
	{
		glm::mat4              nodeMatrix    = node->getLocalMatrix();
		VulkanglTFModel::Node *currentParent = node->parent;
		while (currentParent)
		{
			nodeMatrix    = currentParent->getLocalMatrix() * nodeMatrix;
			currentParent = currentParent->parent;
		}
		return nodeMatrix;
	}

	// POI: Update the joint matrices from the current animation frame and pass them to the GPU
	void VulkanglTFModel::updateJoints(VulkanglTFModel::Node *node)
	{
		// 如果节点有skin，则更新关节矩阵
		if (node->skin > -1)
		{
			// 更新关节矩阵
			glm::mat4              inverseTransform = glm::inverse(getNodeMatrix(node));
			Skin                   skin             = skins[node->skin];
			size_t                 numJoints        = (uint32_t) skin.joints.size();
			std::vector<glm::mat4> jointMatrices(numJoints);
			for (size_t i = 0; i < numJoints; i++)
			{
				// 使用逆绑定矩阵将顶点坐标从模型空间转换到对应绑定关节的局部空间，然后使用关节矩阵将顶点坐标转换到世界空间
				jointMatrices[i] = getNodeMatrix(skin.joints[i]) * skin.inverseBindMatrices[i];
				// 使用逆变换矩阵将顶点坐标从世界空间转换到mesh模型空间，因为我们后面MVP矩阵是使用mesh模型空间计算的
				jointMatrices[i] = inverseTransform * jointMatrices[i];
			}
			// Update ssbo
			skin.ssbo.copyTo(jointMatrices.data(), jointMatrices.size() * sizeof(glm::mat4));
		}
		// 更每个node的动画矩阵
		animationTransforms.animMatrices[node->index] = getNodeMatrix(node);

		for (auto &child : node->children)
		{
			updateJoints(child);
		}
	}

	// Helper functions for locating glTF nodes

	VulkanglTFModel::Node *VulkanglTFModel::findNode(Node *parent, uint32_t index)
	{
		Node *nodeFound = nullptr;
		if (parent->index == index)
		{
			return parent;
		}
		for (auto &child : parent->children)
		{
			nodeFound = findNode(child, index);
			if (nodeFound)
			{
				break;
			}
		}
		return nodeFound;
	}

	VulkanglTFModel::Node *VulkanglTFModel::nodeFromIndex(uint32_t index)
	{
		Node *nodeFound = nullptr;
		for (auto &node : nodes)
		{
			nodeFound = findNode(node, index);
			if (nodeFound)
			{
				break;
			}
		}
		return nodeFound;
	}

	void loadSkins(tinygltf::Model& input)
	{
		skins.resize(input.skins.size());

		for (size_t i = 0; i < input.skins.size(); i++)
		{
			tinygltf::Skin glTFSkin = input.skins[i];

			skins[i].name = glTFSkin.name;
			// Find the root node of the skeleton
			skins[i].skeletonRoot = nodeFromIndex(glTFSkin.skeleton);

			// Find joint nodes
			for (int jointIndex : glTFSkin.joints)
			{
				Node *node = nodeFromIndex(jointIndex);
				if (node)
				{
					skins[i].joints.push_back(node);
				}
			}

			// Get the inverse bind matrices from the buffer associated to this skin
			if (glTFSkin.inverseBindMatrices > -1)
			{
				const tinygltf::Accessor &  accessor   = input.accessors[glTFSkin.inverseBindMatrices];
				const tinygltf::BufferView &bufferView = input.bufferViews[accessor.bufferView];
				const tinygltf::Buffer &    buffer     = input.buffers[bufferView.buffer];
				skins[i].inverseBindMatrices.resize(accessor.count);
				memcpy(skins[i].inverseBindMatrices.data(), &buffer.data[accessor.byteOffset + bufferView.byteOffset], accessor.count * sizeof(glm::mat4));

				// Store inverse bind matrices for this skin in a shader storage buffer object
				// To keep this sample simple, we create a host visible shader storage buffer
				VK_CHECK_RESULT(vulkanDevice->createBuffer(
					VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
					VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
					&skins[i].ssbo,
					sizeof(glm::mat4) * skins[i].inverseBindMatrices.size(),
					skins[i].inverseBindMatrices.data()));
				VK_CHECK_RESULT(skins[i].ssbo.map());
			}
		}
	}

void VulkanglTFModel::PrepareAnimBuffers(tinygltf::Model &input) {
	animationTransforms.animMatrices.resize(input.nodes.size());
	
	// 确保至少有一个矩阵，避免空缓冲区
	if (animationTransforms.animMatrices.empty()) {
		animationTransforms.animMatrices.resize(1);
		animationTransforms.animMatrices[0] = glm::mat4(1.0f);
	}
	
	// 初始化所有矩阵为单位矩阵
	for (auto& matrix : animationTransforms.animMatrices) {
		matrix = glm::mat4(1.0f);
	}
	
	// 创建动画矩阵的SSBO
	VK_CHECK_RESULT(vulkanDevice->createBuffer(
		VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
		VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
		&animationTransforms.ssbo,
		sizeof(glm::mat4) * animationTransforms.animMatrices.size(),
		animationTransforms.animMatrices.data()));
	VK_CHECK_RESULT(animationTransforms.ssbo.map());

	animationTransforms.ssbo.copyTo(animationTransforms.animMatrices.data(), animationTransforms.animMatrices.size() * sizeof(glm::mat4));
}
// POI: Load the animations from the glTF model
void VulkanglTFModel::loadAnimations(tinygltf::Model &input)
{
	animations.resize(input.animations.size());

	for (size_t i = 0; i < input.animations.size(); i++)
	{
		tinygltf::Animation glTFAnimation = input.animations[i];
		animations[i].name                = glTFAnimation.name;

		// Samplers
		animations[i].samplers.resize(glTFAnimation.samplers.size());
		for (size_t j = 0; j < glTFAnimation.samplers.size(); j++)
		{
			tinygltf::AnimationSampler glTFSampler = glTFAnimation.samplers[j];
			AnimationSampler &         dstSampler  = animations[i].samplers[j];
			dstSampler.interpolation               = glTFSampler.interpolation;

			// Read sampler keyframe input time values
			{
				const tinygltf::Accessor &  accessor   = input.accessors[glTFSampler.input];
				const tinygltf::BufferView &bufferView = input.bufferViews[accessor.bufferView];
				const tinygltf::Buffer &    buffer     = input.buffers[bufferView.buffer];
				const void *                dataPtr    = &buffer.data[accessor.byteOffset + bufferView.byteOffset];
				const float *               buf        = static_cast<const float *>(dataPtr);
				for (size_t index = 0; index < accessor.count; index++)
				{
					dstSampler.inputs.push_back(buf[index]);
				}
				// Adjust animation's start and end times
				for (auto input : animations[i].samplers[j].inputs)
				{
					if (input < animations[i].start)
					{
						animations[i].start = input;
					};
					if (input > animations[i].end)
					{
						animations[i].end = input;
					}
				}
			}

			// Read sampler keyframe output translate/rotate/scale values
			{
				const tinygltf::Accessor &  accessor   = input.accessors[glTFSampler.output];
				const tinygltf::BufferView &bufferView = input.bufferViews[accessor.bufferView];
				const tinygltf::Buffer &    buffer     = input.buffers[bufferView.buffer];
				const void *                dataPtr    = &buffer.data[accessor.byteOffset + bufferView.byteOffset];
				switch (accessor.type)
				{
					case TINYGLTF_TYPE_VEC3: {
						const glm::vec3 *buf = static_cast<const glm::vec3 *>(dataPtr);
						for (size_t index = 0; index < accessor.count; index++)
						{
							dstSampler.outputsVec4.push_back(glm::vec4(buf[index], 0.0f));
						}
						break;
					}
					case TINYGLTF_TYPE_VEC4: {
						const glm::vec4 *buf = static_cast<const glm::vec4 *>(dataPtr);
						for (size_t index = 0; index < accessor.count; index++)
						{
							dstSampler.outputsVec4.push_back(buf[index]);
						}
						break;
					}
					default: {
						std::cout << "unknown type" << std::endl;
						break;
					}
				}
			}
		}

		// Channels
		animations[i].channels.resize(glTFAnimation.channels.size());
		for (size_t j = 0; j < glTFAnimation.channels.size(); j++)
		{
			tinygltf::AnimationChannel glTFChannel = glTFAnimation.channels[j];
			AnimationChannel &         dstChannel  = animations[i].channels[j];
			dstChannel.path                        = glTFChannel.target_path;
			dstChannel.samplerIndex                = glTFChannel.sampler;
			dstChannel.node                        = nodeFromIndex(glTFChannel.target_node);
		}
	}
}


// POI: Update the current animation
void VulkanglTFModel::updateAnimation(float deltaTime)
{
	if (activeAnimation > static_cast<uint32_t>(animations.size()) - 1)
	{
		std::cout << "No animation with index " << activeAnimation << std::endl;
		return;
	}
	Animation &animation = animations[activeAnimation];
	animation.currentTime += deltaTime;
	if (animation.currentTime > animation.end)
	{
		animation.currentTime -= animation.end;
	}

	for (auto &channel : animation.channels)
	{
		AnimationSampler &sampler = animation.samplers[channel.samplerIndex];
		for (size_t i = 0; i < sampler.inputs.size() - 1; i++)
		{
			if (sampler.interpolation != "LINEAR")
			{
				std::cout << "This sample only supports linear interpolations\n";
				continue;
			}

			// Get the input keyframe values for the current time stamp
			if ((animation.currentTime >= sampler.inputs[i]) && (animation.currentTime <= sampler.inputs[i + 1]))
			{
				float a = (animation.currentTime - sampler.inputs[i]) / (sampler.inputs[i + 1] - sampler.inputs[i]);
				if (channel.path == "translation")
				{
					channel.node->translation = glm::mix(sampler.outputsVec4[i], sampler.outputsVec4[i + 1], a);
				}
				if (channel.path == "rotation")
				{
					glm::quat q1;
					q1.x = sampler.outputsVec4[i].x;
					q1.y = sampler.outputsVec4[i].y;
					q1.z = sampler.outputsVec4[i].z;
					q1.w = sampler.outputsVec4[i].w;

					glm::quat q2;
					q2.x = sampler.outputsVec4[i + 1].x;
					q2.y = sampler.outputsVec4[i + 1].y;
					q2.z = sampler.outputsVec4[i + 1].z;
					q2.w = sampler.outputsVec4[i + 1].w;

					channel.node->rotation = glm::normalize(glm::slerp(q1, q2, a));
				}
				if (channel.path == "scale")
				{
					channel.node->scale = glm::mix(sampler.outputsVec4[i], sampler.outputsVec4[i + 1], a);
				}
			}
		}
	}
			for (auto &node : nodes)
		{
			updateJoints(node);
		}
		// 将更新后的动画矩阵复制到SSBO
		animationTransforms.ssbo.copyTo(animationTransforms.animMatrices.data(), 
			animationTransforms.animMatrices.size() * sizeof(glm::mat4));


}

	/*
		glTF rendering functions
	*/

	// Draw a single node including child nodes (if present)
	void drawNode(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout, VulkanglTFModel::Node* node)
	{
		if (node->mesh.primitives.size() > 0) {
			// 不再使用push constants传递矩阵，改为通过SSBO传递
			// 动画变换矩阵已经在updateAnimation时更新到SSBO中
			
			for (VulkanglTFModel::Primitive& primitive : node->mesh.primitives) {
				if (primitive.indexCount > 0) {
									// Bind the descriptor for the current primitive's material (set 2)
				vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 2, 1, &materials[primitive.materialIndex].descriptorSet, 0, nullptr);

					vkCmdDrawIndexed(commandBuffer, primitive.indexCount, 1, primitive.firstIndex, 0, 0);
				}
			}
		}
		for (auto& child : node->children) {
			drawNode(commandBuffer, pipelineLayout, child);
		}
	}

	// Draw the glTF scene starting at the top-level-nodes
	void draw(VkCommandBuffer commandBuffer, VkPipelineLayout pipelineLayout)
	{
		// All vertices and indices are stored in single buffers, so we only need to bind once
		VkDeviceSize offsets[1] = { 0 };
		vkCmdBindVertexBuffers(commandBuffer, 0, 1, &vertices.buffer, offsets);
		vkCmdBindIndexBuffer(commandBuffer, indices.buffer, 0, VK_INDEX_TYPE_UINT32);
		// Render all nodes at top-level
		for (auto& node : nodes) {
			drawNode(commandBuffer, pipelineLayout, node);
		}
	}

};

class VulkanExample : public VulkanExampleBase
{
public:
	bool wireframe = false;

	VulkanglTFModel glTFModel;

	struct ShaderData {
		vks::Buffer buffer;
		struct Values {
			glm::mat4 projection;
			glm::mat4 model;
			glm::vec4 lightPos = glm::vec4(5.0f, 5.0f, -5.0f, 1.0f);
			glm::vec4 viewPos;
		} values;
	} shaderData;

	struct Pipelines {
		VkPipeline solid;
		VkPipeline wireframe = VK_NULL_HANDLE;
	} pipelines;

	VkPipelineLayout pipelineLayout;
	VkDescriptorSet descriptorSet;

	struct DescriptorSetLayouts {
		VkDescriptorSetLayout matrices;
		VkDescriptorSetLayout textures;
		VkDescriptorSetLayout jointMatrices;
	} descriptorSetLayouts;

	VulkanExample() : VulkanExampleBase(ENABLE_VALIDATION)
	{
		title = "homework1";
		camera.type = Camera::CameraType::lookat;
		camera.flipY = true;
		camera.setPosition(glm::vec3(0.0f, -0.1f, -1.0f));
		camera.setRotation(glm::vec3(0.0f, 45.0f, 0.0f));
		camera.setPerspective(60.0f, (float)width / (float)height, 0.1f, 256.0f);
	}

	~VulkanExample()
	{
		// Clean up used Vulkan resources
		// Note : Inherited destructor cleans up resources stored in base class
		vkDestroyPipeline(device, pipelines.solid, nullptr);
		if (pipelines.wireframe != VK_NULL_HANDLE) {
			vkDestroyPipeline(device, pipelines.wireframe, nullptr);
		}

		vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
		vkDestroyDescriptorSetLayout(device, descriptorSetLayouts.matrices, nullptr);
		vkDestroyDescriptorSetLayout(device, descriptorSetLayouts.textures, nullptr);
		vkDestroyDescriptorSetLayout(device, descriptorSetLayouts.jointMatrices, nullptr);

		shaderData.buffer.destroy();
	}

	virtual void getEnabledFeatures()
	{
		// Fill mode non solid is required for wireframe display
		if (deviceFeatures.fillModeNonSolid) {
			enabledFeatures.fillModeNonSolid = VK_TRUE;
		};
	}

	void buildCommandBuffers()
	{
		VkCommandBufferBeginInfo cmdBufInfo = vks::initializers::commandBufferBeginInfo();

		VkClearValue clearValues[2];
		clearValues[0].color = defaultClearColor;
		clearValues[0].color = { { 0.25f, 0.25f, 0.25f, 1.0f } };;
		clearValues[1].depthStencil = { 1.0f, 0 };

		VkRenderPassBeginInfo renderPassBeginInfo = vks::initializers::renderPassBeginInfo();
		renderPassBeginInfo.renderPass = renderPass;
		renderPassBeginInfo.renderArea.offset.x = 0;
		renderPassBeginInfo.renderArea.offset.y = 0;
		renderPassBeginInfo.renderArea.extent.width = width;
		renderPassBeginInfo.renderArea.extent.height = height;
		renderPassBeginInfo.clearValueCount = 2;
		renderPassBeginInfo.pClearValues = clearValues;

		const VkViewport viewport = vks::initializers::viewport((float)width, (float)height, 0.0f, 1.0f);
		const VkRect2D scissor = vks::initializers::rect2D(width, height, 0, 0);

		for (int32_t i = 0; i < drawCmdBuffers.size(); ++i)
		{
			renderPassBeginInfo.framebuffer = frameBuffers[i];
			VK_CHECK_RESULT(vkBeginCommandBuffer(drawCmdBuffers[i], &cmdBufInfo));
			vkCmdBeginRenderPass(drawCmdBuffers[i], &renderPassBeginInfo, VK_SUBPASS_CONTENTS_INLINE);
			vkCmdSetViewport(drawCmdBuffers[i], 0, 1, &viewport);
			vkCmdSetScissor(drawCmdBuffers[i], 0, 1, &scissor);
			// Bind scene matrices descriptor to set 0
			vkCmdBindDescriptorSets(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 0, 1, &descriptorSet, 0, nullptr);
			// Bind animation transforms descriptor to set 1
			vkCmdBindDescriptorSets(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout, 1, 1, &glTFModel.animationTransforms.descriptorSet, 0, nullptr);
			vkCmdBindPipeline(drawCmdBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, wireframe ? pipelines.wireframe : pipelines.solid);
			glTFModel.draw(drawCmdBuffers[i], pipelineLayout);
			drawUI(drawCmdBuffers[i]);
			vkCmdEndRenderPass(drawCmdBuffers[i]);
			VK_CHECK_RESULT(vkEndCommandBuffer(drawCmdBuffers[i]));
		}
	}

	void loadglTFFile(std::string filename)
	{
		tinygltf::Model glTFInput;
		tinygltf::TinyGLTF gltfContext;
		std::string error, warning;

		this->device = device;

#if defined(__ANDROID__)
		// On Android all assets are packed with the apk in a compressed form, so we need to open them using the asset manager
		// We let tinygltf handle this, by passing the asset manager of our app
		tinygltf::asset_manager = androidApp->activity->assetManager;
#endif
		bool fileLoaded = gltfContext.LoadASCIIFromFile(&glTFInput, &error, &warning, filename);

		// Pass some Vulkan resources required for setup and rendering to the glTF model loading class
		glTFModel.vulkanDevice = vulkanDevice;
		glTFModel.copyQueue = queue;

		std::vector<uint32_t> indexBuffer;
		std::vector<VulkanglTFModel::Vertex> vertexBuffer;

		if (fileLoaded) {
			glTFModel.loadImages(glTFInput);
			glTFModel.loadMaterials(glTFInput);
			glTFModel.loadTextures(glTFInput);
			const tinygltf::Scene& scene = glTFInput.scenes[0];
			for (size_t i = 0; i < scene.nodes.size(); i++) {
				const tinygltf::Node node = glTFInput.nodes[scene.nodes[i]];
				glTFModel.loadNode(node, glTFInput, nullptr, scene.nodes[i], indexBuffer, vertexBuffer);
			}
			glTFModel.loadSkins(glTFInput);
			glTFModel.loadAnimations(glTFInput);
			// 初始化动画缓冲区
			glTFModel.PrepareAnimBuffers(glTFInput);
			// Calculate initial pose
			for (auto node : glTFModel.nodes)
			{
				glTFModel.updateJoints(node);
			}
		}
		else {
			vks::tools::exitFatal("Could not open the glTF file.\n\nThe file is part of the additional asset pack.\n\nRun \"download_assets.py\" in the repository root to download the latest version.", -1);
			return;
		}

		// Create and upload vertex and index buffer
		// We will be using one single vertex buffer and one single index buffer for the whole glTF scene
		// Primitives (of the glTF model) will then index into these using index offsets

		size_t vertexBufferSize = vertexBuffer.size() * sizeof(VulkanglTFModel::Vertex);
		size_t indexBufferSize = indexBuffer.size() * sizeof(uint32_t);
		glTFModel.indices.count = static_cast<uint32_t>(indexBuffer.size());

		struct StagingBuffer {
			VkBuffer buffer;
			VkDeviceMemory memory;
		} vertexStaging, indexStaging;

		// Create host visible staging buffers (source)
		VK_CHECK_RESULT(vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			vertexBufferSize,
			&vertexStaging.buffer,
			&vertexStaging.memory,
			vertexBuffer.data()));
		// Index data
		VK_CHECK_RESULT(vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			indexBufferSize,
			&indexStaging.buffer,
			&indexStaging.memory,
			indexBuffer.data()));

		// Create device local buffers (target)
		VK_CHECK_RESULT(vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
			vertexBufferSize,
			&glTFModel.vertices.buffer,
			&glTFModel.vertices.memory));
		VK_CHECK_RESULT(vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
			VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
			indexBufferSize,
			&glTFModel.indices.buffer,
			&glTFModel.indices.memory));

		// Copy data from staging buffers (host) do device local buffer (gpu)
		VkCommandBuffer copyCmd = vulkanDevice->createCommandBuffer(VK_COMMAND_BUFFER_LEVEL_PRIMARY, true);
		VkBufferCopy copyRegion = {};

		copyRegion.size = vertexBufferSize;
		vkCmdCopyBuffer(
			copyCmd,
			vertexStaging.buffer,
			glTFModel.vertices.buffer,
			1,
			&copyRegion);

		copyRegion.size = indexBufferSize;
		vkCmdCopyBuffer(
			copyCmd,
			indexStaging.buffer,
			glTFModel.indices.buffer,
			1,
			&copyRegion);

		vulkanDevice->flushCommandBuffer(copyCmd, queue, true);

		// Free staging resources
		vkDestroyBuffer(device, vertexStaging.buffer, nullptr);
		vkFreeMemory(device, vertexStaging.memory, nullptr);
		vkDestroyBuffer(device, indexStaging.buffer, nullptr);
		vkFreeMemory(device, indexStaging.memory, nullptr);
	}

	void loadAssets()
	{
		loadglTFFile(getAssetPath() + "buster_drone/busterDrone.gltf");
	}

	void setupDescriptors()
	{
		/*
			This sample uses separate descriptor sets (and layouts) for the matrices and materials (textures)
		*/

		std::vector<VkDescriptorPoolSize> poolSizes = {
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1),
			// Five combined image samplers per material (baseColor, normal, metallicRoughness, occlusion, emissive)
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, static_cast<uint32_t>(glTFModel.materials.size()) * 5),
			// One storage buffer for animation transforms
			vks::initializers::descriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1),
		};
		// One set for matrices, one for animation transforms, and one per material
		const uint32_t maxSetCount = static_cast<uint32_t>(glTFModel.materials.size()) + 2;
		VkDescriptorPoolCreateInfo descriptorPoolInfo = vks::initializers::descriptorPoolCreateInfo(poolSizes, maxSetCount);
		VK_CHECK_RESULT(vkCreateDescriptorPool(device, &descriptorPoolInfo, nullptr, &descriptorPool));

		// Descriptor set layout for passing matrices
		VkDescriptorSetLayoutBinding setLayoutBinding = vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, VK_SHADER_STAGE_VERTEX_BIT, 0);
		VkDescriptorSetLayoutCreateInfo descriptorSetLayoutCI = vks::initializers::descriptorSetLayoutCreateInfo(&setLayoutBinding, 1);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorSetLayoutCI, nullptr, &descriptorSetLayouts.matrices));
		// Descriptor set layout for passing material textures (5 bindings: baseColor, normal, metallicRoughness, occlusion, emissive)
		std::vector<VkDescriptorSetLayoutBinding> textureLayoutBindings = {
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 0), // baseColor
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 1), // normal
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 2), // metallicRoughness
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 3), // occlusion
			vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, VK_SHADER_STAGE_FRAGMENT_BIT, 4)  // emissive
		};
		VkDescriptorSetLayoutCreateInfo textureLayoutCI = vks::initializers::descriptorSetLayoutCreateInfo(textureLayoutBindings.data(), static_cast<uint32_t>(textureLayoutBindings.size()));
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &textureLayoutCI, nullptr, &descriptorSetLayouts.textures));
		// Descriptor set layout for passing skin joint matrices
		setLayoutBinding = vks::initializers::descriptorSetLayoutBinding(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, VK_SHADER_STAGE_VERTEX_BIT, 0);
		VK_CHECK_RESULT(vkCreateDescriptorSetLayout(device, &descriptorSetLayoutCI, nullptr, &descriptorSetLayouts.jointMatrices));
		// Pipeline layout: set 0 = scene matrices, set 1 = animation transforms, set 2 = textures
		std::array<VkDescriptorSetLayout, 3> setLayouts = { descriptorSetLayouts.matrices, descriptorSetLayouts.jointMatrices, descriptorSetLayouts.textures };
		VkPipelineLayoutCreateInfo pipelineLayoutCI= vks::initializers::pipelineLayoutCreateInfo(setLayouts.data(), static_cast<uint32_t>(setLayouts.size()));
		// 不再使用push constants，所有变换矩阵都通过SSBO传递
		VK_CHECK_RESULT(vkCreatePipelineLayout(device, &pipelineLayoutCI, nullptr, &pipelineLayout));

		// Descriptor set for scene matrices
		VkDescriptorSetAllocateInfo allocInfo = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &descriptorSetLayouts.matrices, 1);
		VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &descriptorSet));
		VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(descriptorSet, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 0, &shaderData.buffer.descriptor);
		vkUpdateDescriptorSets(device, 1, &writeDescriptorSet, 0, nullptr);
		// Descriptor sets for materials
		for (auto& material : glTFModel.materials) {
			const VkDescriptorSetAllocateInfo allocInfo = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &descriptorSetLayouts.textures, 1);
			VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo, &material.descriptorSet));
			
			// 准备所有纹理的描述符更新
			std::vector<VkWriteDescriptorSet> writeDescriptorSets;
			
			// 为每个binding准备默认纹理（使用第一个纹理作为fallback）
			VkDescriptorImageInfo* defaultImageInfo = glTFModel.images.empty() ? nullptr : &glTFModel.images[0].texture.descriptor;
			
			// Binding 0: BaseColor
			if (material.baseColorTextureIndex < glTFModel.textures.size()) {
				VulkanglTFModel::Texture texture = glTFModel.textures[material.baseColorTextureIndex];
				if (texture.imageIndex < glTFModel.images.size()) {
					VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 0, &glTFModel.images[texture.imageIndex].texture.descriptor);
					writeDescriptorSets.push_back(writeDescriptorSet);
				}
			} else if (defaultImageInfo) {
				VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 0, defaultImageInfo);
				writeDescriptorSets.push_back(writeDescriptorSet);
			}
			
			// Binding 1: Normal
			if (material.normalTextureIndex < glTFModel.textures.size()) {
				VulkanglTFModel::Texture texture = glTFModel.textures[material.normalTextureIndex];
				if (texture.imageIndex < glTFModel.images.size()) {
					VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, &glTFModel.images[texture.imageIndex].texture.descriptor);
					writeDescriptorSets.push_back(writeDescriptorSet);
				}
			} else if (defaultImageInfo) {
				VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, defaultImageInfo);
				writeDescriptorSets.push_back(writeDescriptorSet);
			}
			
			// Binding 2: MetallicRoughness
			if (material.metallicRoughnessTextureIndex < glTFModel.textures.size()) {
				VulkanglTFModel::Texture texture = glTFModel.textures[material.metallicRoughnessTextureIndex];
				if (texture.imageIndex < glTFModel.images.size()) {
					VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 2, &glTFModel.images[texture.imageIndex].texture.descriptor);
					writeDescriptorSets.push_back(writeDescriptorSet);
				}
			} else if (defaultImageInfo) {
				VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 2, defaultImageInfo);
				writeDescriptorSets.push_back(writeDescriptorSet);
			}
			
			// Binding 3: Occlusion
			if (material.occlusionTextureIndex < glTFModel.textures.size()) {
				VulkanglTFModel::Texture texture = glTFModel.textures[material.occlusionTextureIndex];
				if (texture.imageIndex < glTFModel.images.size()) {
					VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 3, &glTFModel.images[texture.imageIndex].texture.descriptor);
					writeDescriptorSets.push_back(writeDescriptorSet);
				}
			} else if (defaultImageInfo) {
				VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 3, defaultImageInfo);
				writeDescriptorSets.push_back(writeDescriptorSet);
			}
			
			// Binding 4: Emissive
			if (material.emissiveTextureIndex < glTFModel.textures.size()) {
				VulkanglTFModel::Texture texture = glTFModel.textures[material.emissiveTextureIndex];
				if (texture.imageIndex < glTFModel.images.size()) {
					VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 4, &glTFModel.images[texture.imageIndex].texture.descriptor);
					writeDescriptorSets.push_back(writeDescriptorSet);
				}
			} else if (defaultImageInfo) {
				VkWriteDescriptorSet writeDescriptorSet = vks::initializers::writeDescriptorSet(material.descriptorSet, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 4, defaultImageInfo);
				writeDescriptorSets.push_back(writeDescriptorSet);
			}
			
			// 批量更新所有描述符
			if (!writeDescriptorSets.empty()) {
				vkUpdateDescriptorSets(device, static_cast<uint32_t>(writeDescriptorSets.size()), writeDescriptorSets.data(), 0, nullptr);
			}
		}

		// Descriptor sets for animation transforms
		if (glTFModel.animationTransforms.ssbo.buffer != VK_NULL_HANDLE) {
			const VkDescriptorSetAllocateInfo allocInfo1 = vks::initializers::descriptorSetAllocateInfo(descriptorPool, &descriptorSetLayouts.jointMatrices, 1);
			VK_CHECK_RESULT(vkAllocateDescriptorSets(device, &allocInfo1, &glTFModel.animationTransforms.descriptorSet));
			VkWriteDescriptorSet writeDescriptorSet1 = vks::initializers::writeDescriptorSet(glTFModel.animationTransforms.descriptorSet, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 0, &glTFModel.animationTransforms.ssbo.descriptor);
			vkUpdateDescriptorSets(device, 1, &writeDescriptorSet1, 0, nullptr);
		} else {
			std::cerr << "Error: Animation transforms SSBO not properly initialized!" << std::endl;
		}
		
	}

	void preparePipelines()
	{
		VkPipelineInputAssemblyStateCreateInfo inputAssemblyStateCI = vks::initializers::pipelineInputAssemblyStateCreateInfo(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, 0, VK_FALSE);
		VkPipelineRasterizationStateCreateInfo rasterizationStateCI = vks::initializers::pipelineRasterizationStateCreateInfo(VK_POLYGON_MODE_FILL, VK_CULL_MODE_BACK_BIT, VK_FRONT_FACE_COUNTER_CLOCKWISE, 0);
		VkPipelineColorBlendAttachmentState blendAttachmentStateCI = vks::initializers::pipelineColorBlendAttachmentState(0xf, VK_FALSE);
		VkPipelineColorBlendStateCreateInfo colorBlendStateCI = vks::initializers::pipelineColorBlendStateCreateInfo(1, &blendAttachmentStateCI);
		VkPipelineDepthStencilStateCreateInfo depthStencilStateCI = vks::initializers::pipelineDepthStencilStateCreateInfo(VK_TRUE, VK_TRUE, VK_COMPARE_OP_LESS_OR_EQUAL);
		VkPipelineViewportStateCreateInfo viewportStateCI = vks::initializers::pipelineViewportStateCreateInfo(1, 1, 0);
		VkPipelineMultisampleStateCreateInfo multisampleStateCI = vks::initializers::pipelineMultisampleStateCreateInfo(VK_SAMPLE_COUNT_1_BIT, 0);
		const std::vector<VkDynamicState> dynamicStateEnables = { VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR };
		VkPipelineDynamicStateCreateInfo dynamicStateCI = vks::initializers::pipelineDynamicStateCreateInfo(dynamicStateEnables.data(), static_cast<uint32_t>(dynamicStateEnables.size()), 0);
		// Vertex input bindings and attributes
		const std::vector<VkVertexInputBindingDescription> vertexInputBindings = {
			vks::initializers::vertexInputBindingDescription(0, sizeof(VulkanglTFModel::Vertex), VK_VERTEX_INPUT_RATE_VERTEX),
		};
		const std::vector<VkVertexInputAttributeDescription> vertexInputAttributes = {
			vks::initializers::vertexInputAttributeDescription(0, 0, VK_FORMAT_R32G32B32_SFLOAT, offsetof(VulkanglTFModel::Vertex, pos)),	// Location 0: Position
			vks::initializers::vertexInputAttributeDescription(0, 1, VK_FORMAT_R32G32B32_SFLOAT, offsetof(VulkanglTFModel::Vertex, normal)),// Location 1: Normal
			vks::initializers::vertexInputAttributeDescription(0, 2, VK_FORMAT_R32G32_SFLOAT, offsetof(VulkanglTFModel::Vertex, uv)),	// Location 2: Texture coordinates
			vks::initializers::vertexInputAttributeDescription(0, 3, VK_FORMAT_R32G32B32_SFLOAT, offsetof(VulkanglTFModel::Vertex, color)),	// Location 3: Color
			vks::initializers::vertexInputAttributeDescription(0, 4, VK_FORMAT_R32_UINT, offsetof(VulkanglTFModel::Vertex, nodeIndex)),	// Location 4: Node Index
		};
		VkPipelineVertexInputStateCreateInfo vertexInputStateCI = vks::initializers::pipelineVertexInputStateCreateInfo();
		vertexInputStateCI.vertexBindingDescriptionCount = static_cast<uint32_t>(vertexInputBindings.size());
		vertexInputStateCI.pVertexBindingDescriptions = vertexInputBindings.data();
		vertexInputStateCI.vertexAttributeDescriptionCount = static_cast<uint32_t>(vertexInputAttributes.size());
		vertexInputStateCI.pVertexAttributeDescriptions = vertexInputAttributes.data();

		const std::array<VkPipelineShaderStageCreateInfo, 2> shaderStages = {
			loadShader(getHomeworkShadersPath() + "homework1/mesh.vert.spv", VK_SHADER_STAGE_VERTEX_BIT),
			loadShader(getHomeworkShadersPath() + "homework1/mesh.frag.spv", VK_SHADER_STAGE_FRAGMENT_BIT)
		};

		VkGraphicsPipelineCreateInfo pipelineCI = vks::initializers::pipelineCreateInfo(pipelineLayout, renderPass, 0);
		pipelineCI.pVertexInputState = &vertexInputStateCI;
		pipelineCI.pInputAssemblyState = &inputAssemblyStateCI;
		pipelineCI.pRasterizationState = &rasterizationStateCI;
		pipelineCI.pColorBlendState = &colorBlendStateCI;
		pipelineCI.pMultisampleState = &multisampleStateCI;
		pipelineCI.pViewportState = &viewportStateCI;
		pipelineCI.pDepthStencilState = &depthStencilStateCI;
		pipelineCI.pDynamicState = &dynamicStateCI;
		pipelineCI.stageCount = static_cast<uint32_t>(shaderStages.size());
		pipelineCI.pStages = shaderStages.data();

		// Solid rendering pipeline
		VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCI, nullptr, &pipelines.solid));

		// Wire frame rendering pipeline
		if (deviceFeatures.fillModeNonSolid) {
			rasterizationStateCI.polygonMode = VK_POLYGON_MODE_LINE;
			rasterizationStateCI.lineWidth = 1.0f;
			VK_CHECK_RESULT(vkCreateGraphicsPipelines(device, pipelineCache, 1, &pipelineCI, nullptr, &pipelines.wireframe));
		}
	}

	// Prepare and initialize uniform buffer containing shader uniforms
	void prepareUniformBuffers()
	{
		// Vertex shader uniform buffer block
		VK_CHECK_RESULT(vulkanDevice->createBuffer(
			VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
			VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
			&shaderData.buffer,
			sizeof(shaderData.values)));

		// Map persistent
		VK_CHECK_RESULT(shaderData.buffer.map());

		updateUniformBuffers();
	}

	void updateUniformBuffers()
	{
		shaderData.values.projection = camera.matrices.perspective;
		shaderData.values.model = camera.matrices.view;
		shaderData.values.viewPos = camera.viewPos;
		memcpy(shaderData.buffer.mapped, &shaderData.values, sizeof(shaderData.values));
	}

	void prepare()
	{
		VulkanExampleBase::prepare();
		loadAssets();
		prepareUniformBuffers();
		setupDescriptors();
		preparePipelines();
		buildCommandBuffers();
		prepared = true;
	}

	virtual void render()
	{
		renderFrame();
		if (camera.updated) {
			updateUniformBuffers();
		}
	if (!paused)
	{
		glTFModel.updateAnimation(frameTimer);
	}
	}

	virtual void viewChanged()
	{
		updateUniformBuffers();
	}

	virtual void OnUpdateUIOverlay(vks::UIOverlay *overlay)
	{
		if (overlay->header("Settings")) {
			if (overlay->checkBox("Wireframe", &wireframe)) {
				buildCommandBuffers();
			}
		}
	}
};

VULKAN_EXAMPLE_MAIN()
