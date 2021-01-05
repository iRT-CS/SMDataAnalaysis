import json
import math

# A metric that approximately measures how "volatile" a structure is
    # in other words: how much the layer size changes from layer to layer
def layer_deviation(shape):
    # Add up the differences in the layer sizes
    differences = []
    for i in range(0, len(shape) - 1):
        differences.append(abs(shape[i] - shape[i + 1]))
    if len(differences) == 0:
        return 0

    # The average size of all layers - we want larger shapedevs on structures with relatively smaller layers because
        # the ratio of layer sizes is much greater
        # i.e. [1, 2] is more volatile than [5, 6]
    average = sum(shape) / len(shape)

    # add the max(differences) onto the total sum to weigh the largest difference more
    # divide by the square root of the length of the differences because otherwise smaller structures would have
        # significantly larger shapedevs
    curvedAverageDifference = (sum(differences) + max(differences)) / (math.sqrt(len(differences)) + 1)

    # Round the quotient and return
    return round(curvedAverageDifference / math.sqrt(average) * 100)/100.0


# -------------------------
# Read all of the shapes from the file and write the deviations to a different file
f = open("shapes.json", "r")
fwrite = open("shapeDevs.json", "w")

shapeDevs = {}
line = f.readline()
while line:
    shape = json.loads(line)
    shapeDevs[str(shape)] = layer_deviation(shape)
    line = f.readline()

f.close()
fwrite.write(json.dumps(shapeDevs).replace(", \"", ",\n\""))
fwrite.close()
# -------------------------