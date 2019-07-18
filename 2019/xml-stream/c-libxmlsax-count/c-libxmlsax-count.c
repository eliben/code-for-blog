#include <libxml/parser.h>
#include <stdio.h>
#include <string.h>

typedef struct {
  int counter;
  int inLocation;
  char buf[8192];
} CountState;

void start_document_cb(void* ctx) {
  CountState* state = (CountState*)ctx;
  state->counter = 0;
  state->inLocation = 0;
}

void start_element_cb(void* ctx, const xmlChar* name, const xmlChar** attrs) {
  CountState* state = (CountState*)ctx;

  if (!strcmp((const char*)name, "location")) {
    state->inLocation = 1;
  } else {
    state->inLocation = 0;
  }
}

void end_element_cb(void* ctx, const xmlChar* name) {
  CountState* state = (CountState*)ctx;
  state->inLocation = 0;
}

void characters_cb(void* ctx, const xmlChar* ch, int len) {
  CountState* state = (CountState*)ctx;

  // ch is not NULL-terminated, so we'll copy it to a temporary NULL-terminated
  // buffer stored in 'state'.
  if (len - 1 > sizeof(state->buf)) {
    fprintf(stderr, "characters too long: %d\n", len);
    exit(1);
  }

  memcpy(state->buf, ch, len);
  state->buf[len] = 0;

  if (state->inLocation) {
    if (strstr(state->buf, "Africa")) {
      state->counter++;
    }
  }
}

int main(int argc, char** argv) {
  if (argc != 2) {
    return 1;
  }

	// Initialize libxml and check for potential ABI mismatches with the shared
	// library.
  LIBXML_TEST_VERSION

  xmlSAXHandler handler = {0};
  handler.startDocument = start_document_cb;
  handler.startElement = start_element_cb;
  handler.endElement = end_element_cb;
  handler.characters = characters_cb;

  // Start with empty state; it will be initialized by the startDocument
  // callback.
  CountState count_state;

  if (xmlSAXUserParseFile(&handler, &count_state, argv[1]) != 0) {
		xmlError* err = xmlGetLastError();
    printf("Error parsing %s: line %d, error: %s\n",
				argv[1], err->line, err->message);
    return 1;
  }

  printf("count = %d\n", count_state.counter);

  xmlCleanupParser();
  return 0;
}
