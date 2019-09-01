#version 110

uniform sampler2D current_frame;
uniform sampler2D last_frame;
uniform sampler1D table;
uniform float alpha;
varying vec2 texcoord;

void main()
{

    vec4 current_index = texture2D(current_frame, texcoord);
    vec4 last_index = texture2D(last_frame, texcoord);
    gl_FragColor = alpha*texture1D(table, current_index.x)+(1.0-alpha)*texture1D(table, last_index.x);
}

