#version 450

layout (set = 2, binding = 0) uniform sampler2D samplerColorMap;
layout (set = 2, binding = 1) uniform sampler2D samplerNormalMap;
layout (set = 2, binding = 2) uniform sampler2D samplerMetallicRoughnessMap;
layout (set = 2, binding = 3) uniform sampler2D samplerOcclusionMap;
layout (set = 2, binding = 4) uniform sampler2D samplerEmissiveMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inViewVec;
layout (location = 4) in vec3 inLightVec;

layout (location = 0) out vec4 outFragColor;

#define PI 3.1415926535897932384626433832795

// Normal Distribution function --------------------------------------
float D_GGX(float dotNH, float roughness)
{
	float alpha = roughness * roughness;
	float alpha2 = alpha * alpha;
	float denom = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
	return (alpha2)/(PI * denom*denom); 
}

// Geometric Shadowing function --------------------------------------
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	float r = (roughness + 1.0);
	float k = (r*r) / 8.0;
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

// Fresnel function ----------------------------------------------------
vec3 F_Schlick(float cosTheta, float metallic)
{
	vec3 F0 = mix(vec3(0.04), inColor, metallic); // * material.specular
	vec3 F = F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0); 
	return F;    
}

// Specular BRDF composition --------------------------------------------

vec3 BRDF(vec3 L, vec3 V, vec3 N, float metallic, float roughness)
{
	// Precalculate vectors and dot products	
	vec3 H = normalize (V + L);
	float dotNV = clamp(dot(N, V), 0.0, 1.0);
	float dotNL = clamp(dot(N, L), 0.0, 1.0);
	float dotLH = clamp(dot(L, H), 0.0, 1.0);
	float dotNH = clamp(dot(N, H), 0.0, 1.0);

	// Light color fixed
	vec3 lightColor = vec3(1.0);

	vec3 color = vec3(0.0);

	if (dotNL > 0.0)
	{
		float rroughness = max(0.05, roughness);
		// D = Normal distribution (Distribution of the microfacets)
		float D = D_GGX(dotNH, roughness); 
		// G = Geometric shadowing term (Microfacets shadowing)
		float G = G_SchlicksmithGGX(dotNL, dotNV, rroughness);
		// F = Fresnel factor (Reflectance depending on angle of incidence)
		vec3 F = F_Schlick(dotNV, metallic);

		vec3 spec = D * F * G / (4.0 * dotNL * dotNV);

		color += spec * dotNL * lightColor;
	}

	return color;
}

void main() 
{
	vec4 color = texture(samplerColorMap, inUV) * vec4(inColor, 1.0);
	vec4 normal = texture(samplerNormalMap, inUV);
	vec4 metallicRoughness = texture(samplerMetallicRoughnessMap, inUV);
	vec4 occlusion = texture(samplerOcclusionMap, inUV);
	vec4 emissive = texture(samplerEmissiveMap, inUV);
	vec3 lightColor = vec3(1.0);

	vec3 N = normalize(inNormal);
	vec3 L = normalize(inLightVec);
	vec3 V = normalize(inViewVec);
	vec3 R = reflect(L, N);

	// Fresnel
	vec3 F = F_Schlick(dot(N, V), metallicRoughness.g);

	// BRDF light
	vec3 brdf = BRDF(L, V, N, metallicRoughness.g, metallicRoughness.b);

	// PBR specular
	vec3 specular = brdf;
	// PBR diffuse
	vec3 diffuse = (1.0 - F) * (1.0 - metallicRoughness.g) * color.rgb / PI * dot(N, L) * lightColor;
	vec3 Lo = diffuse + specular;
	//ambient
	vec3 ambient = vec3(0.02) * color.rgb * occlusion.r;
	Lo += ambient;

	//tone mapping
	vec3 toneMapped = Lo / (Lo + vec3(1.0));

	//gamma correction
	toneMapped = pow(toneMapped, vec3(1.0 / 2.2));
	outFragColor = vec4(toneMapped, 1.0);		
}