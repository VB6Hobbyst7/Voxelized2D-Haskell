
#include <math.h>

#include <glad.h>

#include <glfw3.h>

#include <stdio.h>

int inline_c_Graphics_OpenGL_0_fee3d34036aba97ee59677ca0af9323c9b725942() {
return (glfwInit());
}


void * inline_c_Graphics_OpenGL_1_da53656f9f5ac994ddd8f1b73d0f444fd524b1e6(void * ptr_inline_c_0) {
return ( glfwSetErrorCallback ( ptr_inline_c_0 ) );
}


void * inline_c_Graphics_OpenGL_2_db4f395c1c1ae6e1438f3ac6700dbf9f6bb439a3(int w_inline_c_0, int h_inline_c_1, char * n_inline_c_2) {
return (glfwCreateWindow(w_inline_c_0, h_inline_c_1, n_inline_c_2, NULL, NULL));
}


void inline_c_Graphics_OpenGL_3_48c4e70452272da5857dd7db35c0754b23f9f631(void * w_inline_c_0, char c_inline_c_1) {
 glfwSetWindowShouldClose ( w_inline_c_0, c_inline_c_1 )  ;
}


void inline_c_Graphics_OpenGL_4_d59867973c1e305220ce6f4efb5db32299e446f2(void * window_inline_c_0, void * w2_inline_c_1) {
 glfwSetFramebufferSizeCallback ( window_inline_c_0,  w2_inline_c_1 )  ;
}


void inline_c_Graphics_OpenGL_5_1fcf3a2a2eeb7ebf726eb2e10742c916cd30b457(void * window_inline_c_0, void * w2_inline_c_1) {
 glfwSetKeyCallback ( window_inline_c_0,  w2_inline_c_1 )  ;
}


void inline_c_Graphics_OpenGL_6_c3143be6bdd43eb718dd7dcc5dc93a9ca935cf8c(void * window_inline_c_0, void * w2_inline_c_1) {
 glfwSetMouseButtonCallback ( window_inline_c_0,  w2_inline_c_1 )  ;
}


void inline_c_Graphics_OpenGL_7_cb5bb105f88c9f974c67b3e218092c8e6b95d003() {
glfwTerminate();
}


void inline_c_Graphics_OpenGL_8_cd1d4d8d0ea13a15d371aff077011363df1b5602(void * handle_inline_c_0) {
glfwMakeContextCurrent( handle_inline_c_0 );
}


int inline_c_Graphics_OpenGL_9_fc7d1172a02d9d31e36213443ee1e2d590307170(void * handle_inline_c_0) {
return (glfwWindowShouldClose( handle_inline_c_0 ));
}


int inline_c_Graphics_OpenGL_10_5cdd4bceed5f9463a3a557b1ec7618bee5e1fc9e(int c1_inline_c_0, char * c2_inline_c_1) {
return (glGetUniformLocation (c1_inline_c_0, c2_inline_c_1));
}


void inline_c_Graphics_OpenGL_11_c3f2f962641bcbccdef514f82669784e577337d3(int c1_inline_c_0, int c2_inline_c_1) {
glUniform1i ( c1_inline_c_0, c2_inline_c_1 ) ;
}


void inline_c_Graphics_OpenGL_12_ef0d766241dee7a5ce95f74d7673deba6fa82fd5(int c1_inline_c_0, float c2_inline_c_1) {
glUniform1f ( c1_inline_c_0, c2_inline_c_1 ) ;
}


void inline_c_Graphics_OpenGL_13_bfc217a210d157312f3a295dbdc678473988274b(int c1_inline_c_0, float c2_inline_c_1, float c3_inline_c_2) {
glUniform2f ( c1_inline_c_0, c2_inline_c_1, c3_inline_c_2 ) ;
}


void inline_c_Graphics_OpenGL_14_90f1190e28306e9403041c5a715109e7b2e3b7d3(int c1_inline_c_0, float c2_inline_c_1, float c3_inline_c_2, float c4_inline_c_3) {
glUniform3f ( c1_inline_c_0, c2_inline_c_1, c3_inline_c_2, c4_inline_c_3 ) ;
}


void inline_c_Graphics_OpenGL_15_1019537cf2426f4f5f2e606f631ac31e1c36a54f(int c1_inline_c_0, float c2_inline_c_1, float c3_inline_c_2, float c4_inline_c_3, float c5_inline_c_4) {
glUniform4f ( c1_inline_c_0, c2_inline_c_1, c3_inline_c_2, c4_inline_c_3 , c5_inline_c_4) ;
}


void inline_c_Graphics_OpenGL_16_52c671ffdbc38fc2b1810c818a4912bf8fbe340f(int c1_inline_c_0, char c2_inline_c_1, float * ptr_inline_c_2) {
glUniformMatrix4fv ( c1_inline_c_0, 1, c2_inline_c_1,  ptr_inline_c_2) ;
}


void inline_c_Graphics_OpenGL_17_d581474ed01d5f0a353df057f9693f287e0fb4af(int c1_inline_c_0, int * out_inline_c_1) {
glGetIntegerv ( c1_inline_c_0, out_inline_c_1 );
}


void inline_c_Graphics_OpenGL_18_c09408afbf64a10ce5af32cff78ed333ec80b59c(unsigned f_inline_c_0) {
glClear( f_inline_c_0 );
}


void inline_c_Graphics_OpenGL_19_6785b2b671fbbc7963a43b6b141823c3b1eef9a0(float f1_inline_c_0, float f2_inline_c_1, float f3_inline_c_2, float f4_inline_c_3) {
glClearColor( f1_inline_c_0, f2_inline_c_1, f3_inline_c_2, f4_inline_c_3 );
}


void inline_c_Graphics_OpenGL_20_dd06983ca0907dc595c3f5f9bfec1997040a755f(int c1_inline_c_0, int c2_inline_c_1, int c3_inline_c_2, int c4_inline_c_3) {
glViewport (c1_inline_c_0, c2_inline_c_1, c3_inline_c_2, c4_inline_c_3) ;
}


int inline_c_Graphics_OpenGL_21_8fca97e75f7333aaecf73813182d58f886515fdb() {
return (glCreateProgram());
}


int inline_c_Graphics_OpenGL_22_31437bd0dd45dbf2a047723adae95c20e63aa197(int f_inline_c_0) {
return (glCreateShader( f_inline_c_0 ));
}


void inline_c_Graphics_OpenGL_23_1d1ff53ea5c36a571299453c0acd44419e7be270(int h_inline_c_0, int c_inline_c_1, char * str_inline_c_2) {
glShaderSource( h_inline_c_0, c_inline_c_1, & str_inline_c_2, NULL);
}


void inline_c_Graphics_OpenGL_24_1c2d04a548ea1fc949be1fd4b8bfefb016dd83a9(int h_inline_c_0) {
glCompileShader( h_inline_c_0 );
}


void inline_c_Graphics_OpenGL_25_2db0e9fdc59c3042060a6085adf716948e398568(int s_inline_c_0, int m_inline_c_1, int * result_inline_c_2) {
glGetShaderiv( s_inline_c_0, m_inline_c_1, result_inline_c_2);
}


void inline_c_Graphics_OpenGL_26_2e5aed9b74e58d2b032a9cf0c71cc976518803a4(int * val_inline_c_0) {
glGenVertexArrays( 1, val_inline_c_0 );
}


void inline_c_Graphics_OpenGL_27_75653890e37c5649f8dddd01d9d5d0f9e5e3d89a(int * val_inline_c_0) {
glGenBuffers( 1, val_inline_c_0 );
}


void inline_c_Graphics_OpenGL_28_8c30fd7bd4d66d6d41abace5ca47a76225503384(int con_inline_c_0) {
glBindVertexArray( con_inline_c_0 );
}


void inline_c_Graphics_OpenGL_29_e6e8d53087ac9c93d9866b6377699ecc42609b87(int con1_inline_c_0, int con2_inline_c_1) {
glBindBuffer( con1_inline_c_0 , con2_inline_c_1);
}


void inline_c_Graphics_OpenGL_30_7a44555dba3fa95a68cc85dc2b24123965cff1b1(int con1_inline_c_0, int con2_inline_c_1, void * dat_inline_c_2, int con3_inline_c_3) {
glBufferData( con1_inline_c_0 , con2_inline_c_1, dat_inline_c_2, con3_inline_c_3);
}


void inline_c_Graphics_OpenGL_31_6510368ae469e4c99c02dc8afd80b250ee075726(int con1_inline_c_0) {
glDeleteVertexArrays(1, & con1_inline_c_0);
}


void inline_c_Graphics_OpenGL_32_1aa7beba1c3f9b841f072f8744b16cb630ad4de6(int con1_inline_c_0) {
glDeleteBuffers(1, & con1_inline_c_0);
}


void inline_c_Graphics_OpenGL_33_b10a67c9a9e0bdccda15ad678d47581c17e45fa1(int con1_inline_c_0, int con2_inline_c_1, int con3_inline_c_2, void * indices_inline_c_3) {
glDrawElements( con1_inline_c_0, con2_inline_c_1, con3_inline_c_2, indices_inline_c_3);
}


void inline_c_Graphics_OpenGL_34_a970403317152714ae09c062e7a0fba997d0d862(int con1_inline_c_0, int con2_inline_c_1, int con3_inline_c_2, char con4_inline_c_3, int con5_inline_c_4, long con6_inline_c_5) {
glVertexAttribPointer( con1_inline_c_0, con2_inline_c_1, con3_inline_c_2, con4_inline_c_3, con5_inline_c_4, con6_inline_c_5);
}


void inline_c_Graphics_OpenGL_35_9c29fc60032e670f4f3808f8ddcb9417cfdd60b8(int con1_inline_c_0) {
glEnableVertexAttribArray(con1_inline_c_0);
}


void inline_c_Graphics_OpenGL_36_fb61f8eb2ad6739aa36017499eab9e9a61f017c6(int s_inline_c_0, int c_inline_c_1, char * str_inline_c_2) {
glGetShaderInfoLog( s_inline_c_0, c_inline_c_1, NULL, str_inline_c_2);
}


void inline_c_Graphics_OpenGL_37_4ca00bfce78e35a58ad5c4e7454713960665c087(int p_inline_c_0, int s_inline_c_1) {
glAttachShader( p_inline_c_0, s_inline_c_1);
}


void inline_c_Graphics_OpenGL_38_931441ebfcd0fc375e356e9a2be028d3bc7b347d(int p_inline_c_0) {
glLinkProgram( p_inline_c_0 );
}


void inline_c_Graphics_OpenGL_39_debca14f821ffe578fbd1a644d8c06a8f27ced98(int p_inline_c_0) {
glValidateProgram( p_inline_c_0);
}


void inline_c_Graphics_OpenGL_40_7286441cc5b156de0700ada62b1e2ed2515bc4b1(int p_inline_c_0) {
glUseProgram( p_inline_c_0 );
}


void inline_c_Graphics_OpenGL_41_3bddc9598e1a070bd32160807e3b06e91fe3d569(void * handle_inline_c_0) {
glfwSwapBuffers(  handle_inline_c_0 );
}


void inline_c_Graphics_OpenGL_42_f8d3648b35898cc511aa09659307d930effdb00c() {
glfwPollEvents();
}


int inline_c_Graphics_OpenGL_43_9fa711438a2f561807c3de22852f879532533523() {
return (gladLoadGLLoader((GLADloadproc)glfwGetProcAddress));
}

