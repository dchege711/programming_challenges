from glob import glob

from smooshed_morse_decoder import get_matching_permutations

def main_challenge():
    # Check all the provided inputs
    for input_file_path in glob("./data/*.in"):
        expected_output_file_path = input_file_path.replace(".in", ".ans")
        with open(input_file_path, "r") as input_fp:
            smorse_text = input_fp.readline().strip()
            with open(expected_output_file_path, "r") as ans_fp:
                valid_decoding = ans_fp.readline().strip()
                matches = set(get_matching_permutations(smorse_text))
                assert valid_decoding in matches
                print(f"Passed {input_file_path}")

if __name__ == "__main__":
    main_challenge()
