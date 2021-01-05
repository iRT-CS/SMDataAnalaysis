import json
import numpy as np

MAX_NODES = 6
MAX_LAYERS = 4

IN_SHAPE = (2,)
OUT_SHAPE = (1,)

NODES_INLAYER = 2
NODES_OUTLAYER = 1

# to generate all possible combinations of neural net structures.
def iterate(hiddenLayers,maxLayers,maxNodes):
     curLayer = 0
     while(curLayer<len(hiddenLayers)):
          if(hiddenLayers[curLayer] < maxNodes):
               hiddenLayers[curLayer] += 1
               return hiddenLayers
          else:
               hiddenLayers[curLayer] = 1
               curLayer += 1
     if(curLayer >= maxLayers):
          return -1
     else:
          hiddenLayers = [1]*(curLayer+1)
          # print("[1]*(curLayer+1) = " + str([1]*(curLayer+1)))
          return hiddenLayers

iter = [1]
f = open("shapes.json", "a")

while(iter != -1):
    f.write(json.dumps(iter) + "\n")
    print("iter = " + str(iter))
    iter = iterate(iter,MAX_LAYERS,MAX_NODES)

f.close()