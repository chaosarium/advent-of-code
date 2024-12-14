// search for easter egg: gcc main.c -o main && ./main > egg_search.txt

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

char* read(char* file_name) {
    FILE* f;
    f = fopen(file_name, "r"); 
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* input_text = malloc(fsize + 1);
    fread(input_text, 1, fsize, f);
    fclose(f);

    input_text[fsize] = 0;    
    return input_text;
}

struct robot {
    int xInit;
    int yInit;
    int xVelo;
    int yVelo;
};

struct robot* parse(char* input_text, int* num_bots) {
    
    // count lines
    int num_lines = 0;
    int i = 0;
    while (true) {
        if (input_text[i] == 0) { break; }
        if (input_text[i] == '\n') { 
            num_lines++; 
        }
        i++;
    }
    
    // parse lines
    struct robot* prob = malloc(sizeof(struct robot) * num_lines);
    int bot_id = 0;
    int line_start = 0;
    i = 0;
    while (true) {
        if (input_text[i] == 0) { break; }
        if (input_text[i] == '\n') { 
            int xInit, yInit, xVelo, yVelo;
            sscanf(&input_text[line_start], "p=%d,%d v=%d,%d", &xInit, &yInit, &xVelo, &yVelo);
            prob[bot_id].xInit = xInit;
            prob[bot_id].yInit = yInit;
            prob[bot_id].xVelo = xVelo;
            prob[bot_id].yVelo = yVelo;
            line_start = i + 1;
            bot_id ++;
        }
        i++;
    }
    
    *num_bots = bot_id;
    return prob;
}

int solve(struct robot* prob, int num_bots, int width, int height, int num_ticks) {
    int qTL = 0;
    int qTR = 0;
    int qBL = 0;
    int qBR = 0;
    int xMid = (width / 2);
    int yMid = (height / 2);
    
    int* counts = calloc(sizeof(int), width * height);
    
    for (int bot_id = 0; bot_id < num_bots; bot_id++) {
        struct robot* bot = &prob[bot_id];
        int xFina = ((bot->xInit + (bot->xVelo * num_ticks)) % width + width) % width;
        int yFina = ((bot->yInit + (bot->yVelo * num_ticks)) % height + height) % height;
        
        counts[yFina * width + xFina] += 1;
        
        qBL += (xFina < xMid && yFina > yMid);
        qBR += (xFina > xMid && yFina > yMid);
        qTL += (xFina < xMid && yFina < yMid);
        qTR += (xFina > xMid && yFina < yMid);
    }
    
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) { 
            if (i == yMid || j == xMid) {
                printf("* ");  
            } else if (counts[i * width + j] == 0) {
                printf(". ");
            } else {
                printf("%d ", counts[i * width + j]);
            }
        }
        printf("\n");
    }
    
    printf("%d   %d\n", qTL, qTR);
    printf("%d   %d\n", qBL, qBR);
    return qTL * qTR * qBL * qBR;
}

int solve2(struct robot* prob, int num_bots, int width, int height, int num_ticks) {
    int qTL = 0;
    int qTR = 0;
    int qBL = 0;
    int qBR = 0;
    int xMid = (width / 2);
    int yMid = (height / 2);
    
    int* counts = calloc(sizeof(int), width * height);
    
    for (int bot_id = 0; bot_id < num_bots; bot_id++) {
        struct robot* bot = &prob[bot_id];
        int xFina = ((bot->xInit + (bot->xVelo * num_ticks)) % width + width) % width;
        int yFina = ((bot->yInit + (bot->yVelo * num_ticks)) % height + height) % height;
        
        counts[yFina * width + xFina] += 1;
        
        qBL += (xFina < xMid && yFina > yMid);
        qBR += (xFina > xMid && yFina > yMid);
        qTL += (xFina < xMid && yFina < yMid);
        qTR += (xFina > xMid && yFina < yMid);
    }
        
    // xmas tree search...
    int max_consec_curr = 0;
    int max_consec = 0;
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) { 
            if (counts[i * width + j] > 0 && counts[i * width + j + 1] > 0) {
                max_consec_curr += 1;
                if (max_consec_curr > max_consec) {
                    max_consec = max_consec_curr;
                }
            } else {
                max_consec_curr = 0;
            }
        }
    }
    
    if (max_consec > 10) {
        printf("========== step %d ==========", num_ticks);
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) { 
                if (counts[i * width + j] == 0) {
                    printf(" ");
                } else {
                    printf("▮");
                }
            }
            printf("\n");
        }

    }
    
    return qTL * qTR * qBL * qBR;
}

int main() {
    
    {
        printf("=== p1 test ===\n");
        char* input_text = read("test.txt");
        int num_bots;
        struct robot* prob = parse(input_text, &num_bots);
        int sol = solve(prob, num_bots, 11, 7, 100);
        
        printf("solution: %d\n", sol);
    }
    
    {
        printf("=== p1 real ===\n");
        char* input_text = read("input.txt");
        int num_bots;
        struct robot* prob = parse(input_text, &num_bots);
        int sol = solve(prob, num_bots, 101, 103, 100);
        
        printf("solution: %d\n", sol);
    }

    {
        printf("=== p2 real ===\n");
        char* input_text = read("input.txt");
        int num_bots;
        struct robot* prob = parse(input_text, &num_bots);
        for (int i = 0; i < 10000; i++) {
            solve2(prob, num_bots, 101, 103, i);
        }
    }
    
    return 0;
}