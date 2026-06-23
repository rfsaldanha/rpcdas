# Create CID sequences

Helper function to create CID sequences.

## Usage

``` r
cid_seq(a, b)
```

## Arguments

- a:

  character. Starting CID code.

- b:

  character. Ending CID code.

## Value

A character vector.

## Details

'a' letter must be equal to 'b' letter.

## Examples

``` r
# Creates a sequence
cid_seq(a = "A01", b = "A10")
#>  [1] "A01" "A02" "A03" "A04" "A05" "A06" "A07" "A08" "A09" "A10"
```
