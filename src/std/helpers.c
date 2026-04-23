#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <math.h>
#include "raylib.h"

static Texture2D textures[256];
static int texture_count = 0;

void crl_init_window(int w, int h, char* title) {  InitWindow(w, h, title); }
void crl_set_config_flags(unsigned int flags) { SetConfigFlags(flags); }
int crl_window_should_close() { return WindowShouldClose(); }
float crl_get_frame_time() { return GetFrameTime(); }
void crl_close_window() { CloseWindow(); }
int crl_is_key_down(int key) { return IsKeyDown(key); }
void crl_clear_background(unsigned int color) { ClearBackground(*(Color*)&color); }
void crl_draw_rectangle(int x, int y, int w, int h, unsigned int color) { DrawRectangle(x, y, w, h, *(Color*)&color); }
void crl_begin_drawing() { BeginDrawing(); }
void crl_end_drawing() { EndDrawing(); }

void crl_draw_text(char* text, int x, int y, int font_size, unsigned int color) {
    DrawText(text, x, y, font_size, *(Color*)&color);
}

int crl_load_texture(const char* path) {
    textures[texture_count] = LoadTexture(path);
    return texture_count++;
}

void crl_draw_texture(int handle, int x, int y, unsigned int tint) {
    Color c = *(Color*)&tint;
    DrawTexture(textures[handle], x, y, c);
}

void crl_draw_texture_rec(int handle, int src_x, int src_y, int src_w, int src_h, int dst_x, int dst_y, unsigned int tint) {
    Color c = *(Color*)&tint;
    Rectangle src = { src_x, src_y, src_w, src_h };
    Vector2 pos = { dst_x, dst_y };
    DrawTextureRec(textures[handle], src, pos, c);
}

void crl_unload_texture(int handle) {
    UnloadTexture(textures[handle]);
}

static RenderTexture2D canvas;

void crl_init_canvas(int width, int height) {
    canvas = LoadRenderTexture(width, height);
    SetTextureFilter(canvas.texture, TEXTURE_FILTER_POINT);
}

void crl_begin_canvas() {
    BeginTextureMode(canvas);
}

void crl_end_canvas(int screen_width, int screen_height) {
    EndTextureMode();
    BeginDrawing();
    DrawTexturePro(
        canvas.texture,
        (Rectangle){ 0, 0, canvas.texture.width, -canvas.texture.height },
        (Rectangle){ 0, 0, screen_width, screen_height },
        (Vector2){ 0, 0 },
        0.0f,
        WHITE
    );
    EndDrawing();
}

void c_memcpy(void* dest_ptr, void* src_ptr, unsigned long long size) {
    memcpy(dest_ptr, src_ptr, size);
}

void* c_malloc(unsigned long long size) {
    return malloc(size);
}

void c_free(void* ptr) {
    free(ptr);
}

char c_getchar() {
    return getchar();
}

char c_putchar(char chr) {
    return putchar(chr);
}

void c_puts(char* c_str) {
    puts(c_str);
}

void c_printf(char* c_str) {
    printf(c_str);
}

void c_flush() {
    fflush(stdout);
}

int c_system(char* cmd) {
    return system(cmd);
}

int c_usleep(uint microseconds) {
    return usleep(microseconds);
}

char c_getch() {
    struct termios oldt, newt;
    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    char ch = getchar();
    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    return ch;
}

char c_kbhit() {
    struct termios oldt, newt;
    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    int oldf = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, oldf | O_NONBLOCK);
    int ch = getchar();
    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    fcntl(STDIN_FILENO, F_SETFL, oldf);
    if (ch != EOF) { ungetc(ch, stdin); return 1; }
    return 0;
}

double c_sin(double x) {
    return sin(x);
}
double c_cos(double x) {
    return cos(x);
}