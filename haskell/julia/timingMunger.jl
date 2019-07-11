using CSV    # read in a csv
using DataFrames # for rename!
using Query  # Tidyverse
using Plots  # for plotting

### get the data in a data frame
dataFile = "../auto_timings.csv"
df = CSV.File(dataFile) |> DataFrame

### helper functions
id = x -> x

function mungeName(nameString) arr = split(nameString, "/") end

function genCol(df, colName, index, f)
    df[colName] = map(name -> mungeName(name) |> x -> getindex(x,index) |> f, df[:Name])
    df
end

function addData(df) genCol(df, :DataSet, 1, x->x) end

function addAlg(df) genCol(df, :Algorithm, 2, x->x) end

function addNumPlain(df) genCol(df, :PlainCount, 7, x->parse(Float64, x)) end

function addConf(df) genCol(df, :Config, 3, id) end

function addNumChc(df) genCol(df, :ChcCount, 5, x->parse(Int,x)) end

function addChc(df) genCol(df, :Chc, 4, id) end

function addVariantSize(df) genCol(df, :Variant, 6, id) end

function addNumVariant(df) genCol(df, :VariantSize, 7, x->parse(Int,x)) end

function addType(df) genCol(df, :FormulaType, 12, id) end

function addComp(df) genCol(df, :CompressionRatio, 9, x->parse(Float64,x)) end

# mutate the data by splitting on the name column
# function mungeDF!(df::DataFrame)
#     df |>
#         addNumPlain |>
#         addAlg |>
#         # addNumPlain |>
#         addConf |>
#         addNumChc |>
#         addChc |>
#         addVariantSize |>
#         addNumVariant |>
#         addComp |> addType
# end
function mungeDF!(df::DataFrame)
    df |> addData |> addAlg |> addConf |> addNumChc |> addNumPlain |> addComp
end

## perform the mutation
data = mungeDF!(df)

## write the file
data |> CSV.write("../data/auto_data.csv")
