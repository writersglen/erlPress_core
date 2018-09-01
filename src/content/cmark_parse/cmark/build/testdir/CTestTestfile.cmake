# CMake generated Testfile for 
# Source directory: /home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test
# Build directory: /home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(api_test "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/api_test/api_test")
add_test(html_normalization "/usr/bin/python3" "-m" "doctest" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/normalize.py")
add_test(spectest_library "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec_tests.py" "--no-normalize" "--spec" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec.txt" "--library-dir" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src")
add_test(pathological_tests_library "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/pathological_tests.py" "--library-dir" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src")
add_test(roundtriptest_library "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/roundtrip_tests.py" "--spec" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec.txt" "--library-dir" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src")
add_test(entity_library "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/entity_tests.py" "--library-dir" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src")
add_test(spectest_executable "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec_tests.py" "--no-normalize" "--spec" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec.txt" "--program" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src/cmark")
add_test(smartpuncttest_executable "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec_tests.py" "--no-normalize" "--spec" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/smart_punct.txt" "--program" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src/cmark --smart")
add_test(regressiontest_executable "/usr/bin/python3" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/spec_tests.py" "--no-normalize" "--spec" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/test/regression.txt" "--program" "/home/lloyd/erlPress_core/src/content/cmark_parse/cmark/build/testdir/../src/cmark")
