#version 330 core
  
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 color;

varying out vec3 colorFrag;

uniform mat4 P;
uniform mat4 V;

void main()
{
	colorFrag = color;
	vec4 p = vec4(position.x, position.y, position.z, 1.0);
    gl_Position = p * V * P;
}
