#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <math.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <poll.h>
#include <sys/ioctl.h>

int c_bytes_available(int sock) {
    int bytes;
    ioctl(sock, FIONREAD, &bytes);
    return bytes;
}

int c_connect(const char* ip, int port) {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) return -1;
    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    inet_pton(AF_INET, ip, &addr.sin_addr);
    if (connect(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) return -1;
    return sock;
}

int c_listen(int port, int backlog) {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) return -1;
    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) return -1;
    if (listen(sock, backlog) < 0) return -1;
    return sock;
}

int c_accept(int server_sock) {
    return accept(server_sock, NULL, NULL);
}

int c_send(int sock, char* data, long long size) {
    return send(sock, data, size, 0);
}

int c_recv(int sock, char* buf, long long size) {
    return recv(sock, buf, size, 0);
}

void c_close(int sock) {
    close(sock);
}

char* c_read_file(const char* path, long long* out_size) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long long size = ftell(f);
    rewind(f);
    char* buf = malloc(size);
    fread(buf, 1, size, f);
    *out_size = size;
    fclose(f);
    return buf;
}

void c_write_file(const char* path, char* data, long long size) {
    FILE* f = fopen(path, "wb");
    if (!f) return;
    fwrite(data, 1, size, f);
    fclose(f);
}

void c_append_file(const char* path, char* data, long long size) {
    FILE* f = fopen(path, "ab");
    if (!f) return;
    fwrite(data, 1, size, f);
    fclose(f);
}

int c_file_exists(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return 0;
    fclose(f);
    return 1;
}

void c_delete_file(const char* path) {
    remove(path);
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