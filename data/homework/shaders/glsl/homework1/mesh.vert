#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inColor;
layout (location = 4) in uint inNodeIndex;

layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;

layout (set = 1, binding = 0) readonly buffer AnimationMatrices
{
	mat4 nodeMatrices[];
} animationData;

layout (location = 0) out vec3 outNormal;
layout (location = 1) out vec3 outColor;
layout (location = 2) out vec2 outUV;
layout (location = 3) out vec3 outViewVec;
layout (location = 4) out vec3 outLightVec;

void main() 
{
	outNormal = inNormal;
	outColor = inColor;
	outUV = inUV;
	
	mat4 nodeMatrix = animationData.nodeMatrices[inNodeIndex];
	
	gl_Position = uboScene.projection * uboScene.view * nodeMatrix * vec4(inPos.xyz, 1.0);
	
	vec4 pos = uboScene.view * nodeMatrix * vec4(inPos, 1.0);
	outNormal = mat3(uboScene.view * nodeMatrix) * inNormal;
	vec3 lPos = mat3(uboScene.view) * uboScene.lightPos.xyz;
	outLightVec = uboScene.lightPos.xyz - pos.xyz;
	outViewVec = uboScene.viewPos.xyz - pos.xyz;	
}