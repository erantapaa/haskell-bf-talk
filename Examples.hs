module Examples where

import BFInterp

run_baum_sweet input = do
  putStrLn $ "baum_sweet input: " ++ input
  runBFFileWithInput "./examples/baum-sweet.bf" input

run_major_scales input = do
  putStrLn $ "major_scales input: " ++input
  runBFFileWithInput "./examples/major-scales.bf" input

baumSweet_ex1 = run_baum_sweet "20"

major_scales_ex1 = run_major_scales "C Do" 
major_scales_ex2 = run_major_scales "A#   Fa "
major_scales_ex3 = run_major_scales "D  mi"

run_ascii85_decoder input = do
  putStrLn $ "ascii85_decoder input: " ++ input
  runBFFileWithInput "./examples/ascii85-decoder.bf" input

run_ascii85_encoder input = do
  putStrLn $ "ascii85-encoder input: " ++ input
  runBFFileWithInput "./examples/ascii85-encoder.bf" input

ascii85_encoder_ex1 = run_ascii85_encoder "Hello, world!"
-- ascii85_decoder_ex1 = run_ascii85_encoder "Hello, world!"

run_first_recurring_char input = do
  putStrLn $ "first-recurring-char input: " ++ input
  runBFFileWithInput "./examples/first-recurring-char.bf" input

first_recurring_char_ex1 = run_first_recurring_char "IKEUNFUVFV"
first_recurring_char_ex2 = run_first_recurring_char "PXLJOUDJVZGQHLBHGXIW"
first_recurring_char_ex3 = run_first_recurring_char "*l1J?)yn%R[}9~1\"=k7]9;0[$"

