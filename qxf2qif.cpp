/*
 * qxf2qif.c
 *
 * Convert a QXF (OFX/SGML) file to QIF (bank) format.
 *
 * Usage: qxf2qif input.qxf output.qif
 *
 * Simple, robust, ANSI C (C99). Reads entire input file into memory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

#define MAX_FIELD 4096

const char *SW_VERSION =    "1.01";
const char *SW_DATE =       "2025-11-28";

/* Read whole file into a malloc'd buffer. Returns pointer and sets length.
 * Caller must free() returned pointer. Returns NULL on error.
 */
static char *read_file_all(const char *path, long *out_len) {
    FILE *f = fopen(path, "rb");
    char *buf = NULL;
    long len;
    if (!f) return NULL;
    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
    len = ftell(f);
    if (len < 0) { fclose(f); return NULL; }
    if (fseek(f, 0, SEEK_SET) != 0) { fclose(f); return NULL; }
    buf = (char *)malloc(len + 1);
    if (!buf) { fclose(f); return NULL; }
    if (fread(buf, 1, len, f) != (size_t)len) { free(buf); fclose(f); return NULL; }
    buf[len] = '\0';
    fclose(f);
    if (out_len) *out_len = len;
    return buf;
}

/* Case-insensitive search for substring in haystack.
 * Returns pointer to first match or NULL.
 */
static char *strcasestr_simple(const char *hay, const char *needle) {
    size_t nlen = strlen(needle);
    if (nlen == 0) return (char *)hay;
    for (; *hay; hay++) {
        if (tolower((unsigned char)*hay) == tolower((unsigned char)*needle)) {
            if (strncasecmp(hay, needle, nlen) == 0) return (char *)hay;
        }
    }
    return NULL;
}

/* Extract content between <TAG> and </TAG> starting from 'start' pointer.
 * tag should be uppercase tag name (e.g., "DTPOSTED"). Search is case-insensitive.
 * Writes at most out_len-1 bytes to out (null terminated).
 * Returns 1 if found and written, 0 if not found.
 */
static int extract_tag_content(const char *start, const char *tag, char *out, size_t out_len) {
    char opentag[64], closetag[64];
    const char *p, *q;
    size_t tlen = strlen(tag);

    if (tlen + 3 >= sizeof(opentag)) return 0;
    snprintf(opentag, sizeof(opentag), "<%s>", tag);
    snprintf(closetag, sizeof(closetag), "</%s>", tag);

    /* case-insensitive search for opentag */
    p = strcasestr_simple(start, opentag);
    if (!p) {
        /* sometimes OFX uses <TAG>value (no closing tag) or inline like <TAG>value<ANOTHERTAG>
           but in OFX/SGML usually tags are used as <TAG>value</TAG> or just <TAG>value.
           Try a fallback: find "<TAG>" then take chars until '<' (next tag) */
        p = strcasestr_simple(start, opentag);
        if (!p) return 0;
    }

    p += strlen(opentag); /* move to content start */
    q = strcasestr_simple(p, closetag);
    if (q) {
        size_t copylen = (size_t)(q - p);
        if (copylen >= out_len) copylen = out_len - 1;
        memcpy(out, p, copylen);
        out[copylen] = '\0';
        return 1;
    } else {
        /* fallback: copy until next '<' or end */
        const char *r = p;
        while (*r && *r != '<') r++;
        size_t copylen = (size_t)(r - p);
        if (copylen >= out_len) copylen = out_len - 1;
        memcpy(out, p, copylen);
        out[copylen] = '\0';
        return 1;
    }
}

/* Trim leading and trailing whitespace in place */
static void trim_inplace(char *s) {
    char *p = s;
    while (*p && isspace((unsigned char)*p)) p++;
    if (p != s) memmove(s, p, strlen(p) + 1);
    size_t len = strlen(s);
    while (len > 0 && isspace((unsigned char)s[len - 1])) { s[len - 1] = '\0'; len--; }
}

/* Convert OFX date token (YYYYMMDD... ) to MM/DD/YYYY.
 * Accepts tokens beginning with YYYYMMDD (8 chars). If too short, returns 0.
 * Writes to out (outlen should be at least 11) in format "MM/DD/YYYY".
 * Returns 1 on success, 0 on failure.
 */
static int ofxdate_to_mmddyyyy(const char *token, char *out, size_t outlen) {
    if (!token || strlen(token) < 8 || outlen < 11) return 0;
    char yyyy[5], mm[3], dd[3];
    memcpy(yyyy, token + 0, 4); yyyy[4] = '\0';
    memcpy(mm, token + 4, 2); mm[2] = '\0';
    memcpy(dd, token + 6, 2); dd[2] = '\0';
    /* basic validation */
    for (int i = 0; i < 4; i++) if (!isdigit((unsigned char)yyyy[i])) return 0;
    for (int i = 0; i < 2; i++) if (!isdigit((unsigned char)mm[i])) return 0;
    for (int i = 0; i < 2; i++) if (!isdigit((unsigned char)dd[i])) return 0;
    snprintf(out, outlen, "%s/%s/%s", mm, dd, yyyy);
    return 1;
}

/* Find next occurrence of "<STMTTRN" (start tag) in buffer starting at pos.
 * Then find corresponding "</STMTTRN>" end tag and return pointers.
 * Returns 1 if found and sets *startptr and *endptr (endptr points to char after end tag).
 * Returns 0 if no more found.
 */
static int find_next_stmttrn(const char *buf, const char *bufend, const char **startptr, const char **endptr) {
    const char *p = buf;
    const char *open = NULL, *close = NULL;
    p = strcasestr_simple(p, "<STMTTRN");
    if (!p) return 0;
    /* Move to '>' of open tag */
    open = strchr(p, '>');
    if (!open) return 0;
    open++; /* content starts here */
    close = strcasestr_simple(open, "</STMTTRN>");
    if (!close) return 0;
    *startptr = open;
    *endptr = close + strlen("</STMTTRN>");
    return 1;
}

void usage(const char *prog, const char *extraLine = (const char *)(NULL));

void usage(const char *prog, const char *extraLine)
{
    fprintf(stderr, "%s Ver %s %s\n", prog, SW_VERSION, SW_DATE);
    fprintf(stderr, "usage: %s <options>\n", prog);
    fprintf(stderr, "-i --input filename       input .qfx file.\n");
    fprintf(stderr, "                          Extension will be added if not provided.\n");
    fprintf(stderr, "-o --output filename      output .qif file.\n");
    fprintf(stderr, "                          Filename will be generated from input filename\n");
    fprintf(stderr, "                          if not provided.\n");
    fprintf(stderr, "-m --memo                 Include memos.\n");
    fprintf(stderr, "-q --quiet                Quiet running (or decrease verbosity).\n");
    fprintf(stderr, "-v --verbose              Increase verbosity\n");
    if (extraLine) fprintf(stderr, "\n%s\n", extraLine);
}

int main(int argc, char *argv[])
{
    int                 opt;
    char                inFileName[MAX_FIELD];
    char                outFileName[MAX_FIELD];
    bool                usageError = false;
    char                *cp;
    int                 verbosity = 1;
    int                 numTransactions = 0;
    bool                memoFlag = false;
    bool                printMemoWarning = false;

    inFileName[0] = '\0';
    outFileName[0] = '\0';

    struct option longOptions[] =
        {
            {"input",       required_argument,  0,      'i'}
            ,{"output",     required_argument,  0,      'o'}
            ,{"memo",       no_argument,        0,      'm'}
            ,{"quiet",      no_argument,        0,      'q'}
            ,{"verbose",    no_argument,        0,      'v'}
            ,{0,0,0,0}
        };

    while (1)
    {
        int optionIndex = 0;
        opt = getopt_long(argc, argv, "i:o:mqv", longOptions, &optionIndex);

        if (-1 == opt) break;

        switch (opt)
        {
        case 'i':
            strncpy(inFileName, optarg, sizeof(inFileName)-1);
            break;
        case 'o':
            strncpy(outFileName, optarg, sizeof(outFileName)-1);
            break;
        case 'm':
            memoFlag = true;
            break;
        case 'q':
            --verbosity;
            break;
        case 'v':
            ++verbosity;
            break;
        default:
            usageError = true;
            break;
        }
    }

    if (usageError)
    {
        usage(basename(argv[0]));
        return -1;
    }

    // strcpy(inFileName, "/home/bruno/Downloads/transactions.qfx");
    if ('\0' == inFileName[0])
    {
        usage(basename(argv[0]), "Input filename required");
        return -2;
    }

    cp = strchr(inFileName, '.');
    if ((char *)(NULL) == cp)
    {
        // No extension provided.  Add .qfx
        strncat(inFileName, ".qfx", 5);
    }

    if ('\0' == outFileName[0])
    {
        // Create output file name from input file name
        strncpy(outFileName, inFileName, sizeof(outFileName)-1);
        cp = strrchr(outFileName, '.');
        if ((char *)(NULL) == cp)
        {
            // Something went wrong because there should
            // definately be a '.' in the filename
            usage(basename(argv[0]), "Internal error with file names");
            return -3;
        }
        else
        {
            *cp = '\0';
            strncat(outFileName, ".qif", 5);
        }
    }
    else
    {
        // An output file name was provided.
        // See if it has an extension
        cp = strchr(outFileName, '.');
        if ((char *)(NULL) == cp)
        {
            // No extension provided.  Add .qif
            strncat(outFileName, ".qif", 5);
        }
    }
    long len;
    char *buf = read_file_all(inFileName, &len);
    if (!buf) {
        usage("Error reading input file");
        return -4;
    }

    FILE *fout = fopen(outFileName, "w");
    if (!fout) {
        usage("Error opening output file");
        free(buf);
        return -5;
    }

    fprintf(fout, "!Type:Bank\n");

    const char *scan = buf;
    const char *bufend = buf + len;

    while (1) {
        const char *block_start, *block_after;
        if (!find_next_stmttrn(scan, bufend, &block_start, &block_after)) break;

        char dtposted[MAX_FIELD] = {0};
        char trnamt[MAX_FIELD] = {0};
        char name[MAX_FIELD] = {0};
        char memo[MAX_FIELD] = {0};

        /* Extract tags from block_start (which points at content after opening <STMTTRN>) */
        extract_tag_content(block_start, "DTPOSTED", dtposted, sizeof(dtposted));
        extract_tag_content(block_start, "TRNAMT", trnamt, sizeof(trnamt));
        extract_tag_content(block_start, "NAME", name, sizeof(name));
        extract_tag_content(block_start, "MEMO", memo, sizeof(memo));

        trim_inplace(dtposted);
        trim_inplace(trnamt);
        trim_inplace(name);
        trim_inplace(memo);

        /* sanitize name and memo: remove newlines */
        for (char *p = name; *p; ++p) if (*p == '\r' || *p == '\n') *p = ' ';
        for (char *p = memo; *p; ++p) if (*p == '\r' || *p == '\n') *p = ' ';

        /* convert date */
        char qifdate[16] = {0};
        if (!ofxdate_to_mmddyyyy(dtposted, qifdate, sizeof(qifdate))) {
            /* try alternate: maybe date is like YYYYMMDD (8 chars) or YYYYMMDDHHMMSS. If still fails, skip */
            /* Skip this transaction if no valid date; but still attempt to use DTPOSTED raw first 8 */
            if (strlen(dtposted) >= 8) {
                char tmp[16];
                strncpy(tmp, dtposted, 8); tmp[8] = '\0';
                if (!ofxdate_to_mmddyyyy(tmp, qifdate, sizeof(qifdate))) {
                    /* failed */
                    qifdate[0] = '\0';
                }
            }
        }

        /* require at least an amount; skip if none */
        if (trnamt[0] == '\0') {
            scan = block_after;
            continue;
        }

        /* ensure amount uses point (OFX does) and no commas; strip commas (just in case) */
        char amt_clean[MAX_FIELD];
        size_t ai = 0;
        for (size_t i = 0; trnamt[i] && ai + 1 < sizeof(amt_clean); ++i) {
            if (trnamt[i] == ',') continue;
            amt_clean[ai++] = trnamt[i];
        }
        amt_clean[ai] = '\0';

        /* If date conversion failed, use a fallback: print original DTPOSTED */
        if (qifdate[0] == '\0') {
            /* try to use YYYYMMDD -> MM/DD/YYYY as best-effort using first 8 chars */
            if (strlen(dtposted) >= 8) {
                char tmp[9]; memcpy(tmp, dtposted, 8); tmp[8] = '\0';
                if (!ofxdate_to_mmddyyyy(tmp, qifdate, sizeof(qifdate))) {
                    /* give up and skip date field (not ideal) */
                    strncpy(qifdate, dtposted, sizeof(qifdate)-1);
                }
            } else {
                strncpy(qifdate, dtposted, sizeof(qifdate)-1);
            }
        }

        /* QIF: Date (D), Payee/Description (P), Amount (T), Cleared (C*), end(^) */
        if (qifdate[0] != '\0') {
            fprintf(fout, "D%s\n", qifdate);
        } else {
            fprintf(fout, "D\n"); /* empty date (shouldn't happen) */
        }

        /* If name is empty, use a placeholder */
        if (name[0] == '\0') {
            fprintf(fout, "P(unknown)\n");
        } else {
            /* sanitize name: remove internal newlines */
            for (char *p = name; *p; ++p) if (*p == '\r' || *p == '\n') *p = ' ';
            fprintf(fout, "P%s\n", name);
        }

        if (memo[0]) {
            if (memoFlag) {
                fprintf(fout, "M%s\n", memo);  // <-- MEMO line added
            } else {
                printMemoWarning = true;
            }
        }
        fprintf(fout, "T%s\n", amt_clean);
        fprintf(fout, "C*\n");
        fprintf(fout, "^\n");

        ++numTransactions;

        if  (verbosity >= 2)
        {
            if (memo[0] && !memoFlag) {
                strncpy(memo, "EXCLUDED", 9);
            }
            printf("%s\t%.16s\t%.8s\t$%s\n", qifdate, name, memo, amt_clean);
        }

        scan = block_after;
    }

    fclose(fout);
    free(buf);

    if (verbosity >= 1)
    {
        printf("Input File            : %s\n", inFileName);
        printf("Output File           : %s\n", outFileName);
        printf("Number of Transactions: %d\n", numTransactions);
    }

    if (printMemoWarning)
    {
        fprintf(stderr, "Memos appear in input file but are excluded from output.\n");
        fprintf(stderr, "Use -m to include memos in output.\n");
    }

    return 0;
}
