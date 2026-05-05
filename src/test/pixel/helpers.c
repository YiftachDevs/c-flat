#include "raylib.h"
#include <stdio.h>

static Texture2D textures[256];
static int texture_count = 0;

static Font g_font;

int crl_is_mouse_button_down(int button) { return IsMouseButtonDown(button); }
int crl_is_mouse_button_pressed(int button) { return IsMouseButtonPressed(button); }
int crl_is_mouse_button_released(int button) { return IsMouseButtonReleased(button); }
int crl_get_mouse_x() { return GetMouseX(); }
int crl_get_mouse_y() { return GetMouseY(); }
Vector2 crl_get_window_position() { return GetWindowPosition(); }
void crl_set_window_position(int x, int y) { SetWindowPosition(x, y); }

void crl_init_window(int w, int h, char* title) {
    SetConfigFlags(FLAG_WINDOW_UNDECORATED);
    InitWindow(w, h, title);
    g_font = LoadFontEx("pixel/res/PixelCode.ttf", 12, 0, 0);
}



void crl_set_config_flags(unsigned int flags) { SetConfigFlags(flags); }
int crl_window_should_close() { return WindowShouldClose(); }
float crl_get_frame_time() { return GetFrameTime(); }
void crl_close_window() { CloseWindow(); }
int crl_is_key_down(int key) { return IsKeyDown(key); }
void crl_clear_background(unsigned int color) { ClearBackground(*(Color*)&color); }
void crl_draw_rectangle(int x, int y, int w, int h, unsigned int color) { DrawRectangle(x, y, w, h, *(Color*)&color); }
void crl_begin_drawing() { BeginDrawing(); }
void crl_end_drawing() { EndDrawing(); }

void crl_draw_text(char* text, int x, int y, unsigned int color) {
    DrawTextEx(g_font, text, (Vector2){x, y}, 12, 0, *(Color*)&color);
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
