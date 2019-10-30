
int main(int argc, char* argv[]) {
  auto strings = new zenon_string[argc];
  for (int i = 0; i < argc; ++i)
    strings[i] = zenon_string(argv[i]);
  return zenonMain(slice_t<zenon_string>{strings, strings + argc});
}

