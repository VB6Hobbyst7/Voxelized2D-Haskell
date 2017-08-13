
#include <math.h>

#include <glad.h>

#include <glfw3.h>

#include <stdio.h>

int inline_c_Graphics_OpenGL_0_fee3d34036aba97ee59677ca0af9323c9b725942() {
return (glfwInit());
}


long inline_c_Graphics_OpenGL_1_bfc8d954d47603c7bf8c4632b30ce4e75a7d673d(int w_inline_c_0, int h_inline_c_1, char * n_inline_c_2) {
return (glfwCreateWindow(w_inline_c_0, h_inline_c_1, n_inline_c_2, NULL, NULL));
}


void inline_c_Graphics_OpenGL_2_cb5bb105f88c9f974c67b3e218092c8e6b95d003() {
glfwTerminate();
}


void inline_c_Graphics_OpenGL_3_395c2114e03e68af81fac0f5371e29f984255b32(long h_inline_c_0) {
glfwMakeContextCurrent( h_inline_c_0 );
}


int inline_c_Graphics_OpenGL_4_ee0060e965256c0a110a4ea98cbfa6b04726ee55(long h_inline_c_0) {
return (glfwWindowShouldClose( h_inline_c_0 ));
}


void inline_c_Graphics_OpenGL_5_c09408afbf64a10ce5af32cff78ed333ec80b59c(unsigned f_inline_c_0) {
glClear( f_inline_c_0 );
}


void inline_c_Graphics_OpenGL_6_6785b2b671fbbc7963a43b6b141823c3b1eef9a0(float f1_inline_c_0, float f2_inline_c_1, float f3_inline_c_2, float f4_inline_c_3) {
glClearColor( f1_inline_c_0, f2_inline_c_1, f3_inline_c_2, f4_inline_c_3 );
}


void inline_c_Graphics_OpenGL_7_5de2ab3eda38f04f7786178c6a4969cf914e4cea(long h_inline_c_0) {
glfwSwapBuffers( h_inline_c_0 );
}


void inline_c_Graphics_OpenGL_8_f8d3648b35898cc511aa09659307d930effdb00c() {
glfwPollEvents();
}


int inline_c_Graphics_OpenGL_9_9fa711438a2f561807c3de22852f879532533523() {
return (gladLoadGLLoader((GLADloadproc)glfwGetProcAddress));
}

