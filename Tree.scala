import math._


abstract class Node {
    def getStr(level:Int):String
}

// branches have an attribute. Scala puts the constructor
// right in the class definition, and attribute will become
// a member.

case class Branch(val attribute:Symbol) extends Node {
    
    // The children of each branch: a list of (symbol,node) tuples.
    // Note that we don't need to specify the type in the constructor List(),
    // Scala will infer it.
    
    var children: List[Tuple2[Symbol,Node]] = List()
    
    // add (s,n) to the head of the list
    def add(s: Symbol, n: Node) {
        children = (s,n)::children
    }
    
    // recursive method for printing out a tree
    
    override def getStr(level:Int):String = {
        def nodestr(x: Tuple2[Symbol,Node]):String = {
            " "*(level+1)+"Value "+x._1+": "+x._2.getStr(level+2)
        }
        
        " "*level+"Branch "+attribute+"\n"+children.map(x => nodestr(x)).mkString("\n")
    }
}

// leaves have a label

case class Leaf(val label:Symbol) extends Node {
    
    // recursive method for printing out a tree (well, this one's not
    // recursive because it's for the Leaf)
    
    override def getStr(level:Int) :String = {
        " "*level+"Leaf "+label
    }
}


object Tree {
    
    // attributes are a map of names onto lists of possible values
    type Attributes = Map[Symbol,Set[Symbol]]
    // examples are maps of names onto values
    type Example = Map[Symbol,Symbol]
    
    var level:Int =0
    
    def validate(attrs: Attributes,
            examples: List[Example],
            label: Symbol
            )  {
    
        // ensure the label is an attribute
        if(!(attrs.keySet.contains(label))){
            throw new RuntimeException("label %s is not an attribute".format(label));
        }
    
        for(x <- examples){
            // ensure all the attributes are in this example
            if(attrs.keySet != x.keySet){
                throw new RuntimeException("missing attribute in example")
            }
            // and ensure all the values are correct for those attributes
            for(key <- x.keySet){
                if(!attrs(key).contains(x(key))){
                    throw new RuntimeException("bad attribute value for %s".format(key))
                }
            }
        }
    }    
    
    // this is a generic function using a type R which we specify as nullable
    // (so we can return null). It returns the value in the set for which
    // the function on members of that set will have the highest value, by brute force.
    def argmax[R >: Null](attrNames: Set[R], f: R => Double) : R = {
        var max: Double = 0
        var found: R = null
        for(a <- attrNames){
            var v = f(a)
            if(v>max || found==null){
                found = a
                max=v
            }
        }
        return found
    }
    
    def log2(x:Double):Double = {
        return log10(x) / log10(2)
    }
    
    // calculate the entropy of a list of examples with respect to a given label
    
    def entropy(examples: List[Example], label:Symbol, attrs: Attributes) : Double = {
        val size:Double = examples.size
        if(size==0){
            0
        } else {
            val list:List[Symbol] = examples.map(x=>x(label)) // get all the label values in the examples
            // construct a list of counts - need to convert the attribute value set to a list,
            // because otherwise the mapped result would be a set, which would be bad -
            // duplicate values would get folded.
            val labelProportions = attrs(label).toList.map( x => (list.count(y => y==x).toDouble)/size)
            // and do the calc.
            -labelProportions.map( x => if(x==0) 0 else x*log2(x)).sum
        }
    }
    
    
    // determine the information gained by partitioning the examples on the attribute
    // attr.
    
    def informationGain(
            attr: Symbol,
            attrs: Attributes,
            examples: List[Example],
            label: Symbol) : Double = {
    
        // first, we need to calculate the overall entropy of the examples
        val e = entropy(examples,label,attrs)
        
        // then calculate the entropies of the individual subsets.
        
        // First, find the subsets of the examples where the given attribute has
        // each possible value
        
        // what comes out of this has type Set[Tuple2[Symbol,List[Example]]]
        
        val exampleSize:Double = examples.size
        
        def subsetEntropy(subset: List[Example]) : Double = {
            if(exampleSize<1) 0 else (entropy(subset,label,attrs) * (subset.size/exampleSize))
        }
        
        // again, we're mapping over a set which may produce values which are the same; this would
        // result in values being folded away - so before mapping, convert to a list. Note we also
        // remove the label; its subset would have an entropy of zero anyway.
        
        val total = (attrs-label)(attr).toList.map(x => subsetEntropy(examples.filter(y=>y(attr)==x))).sum
        return e-total // substract sum of subset entropies from overall entropy
    }
    
    def levstr = " "*level
    
    def id3(attrs: Attributes,
            examples: List[Example],
            label: Symbol
            ) : Node = {
    
        level = level+1
        // if all the examples have the same label, return a new node with that label
        if(examples.forall( x => x(label) == examples(0)(label))){
            new Leaf(examples(0)(label))
        } else {
            for(a <- attrs.keySet-label){
                println(levstr+"Information gain for %s is %f".format(a,
                              informationGain(a,attrs,examples,label)))
            }
            // find the best splitting attribute - this is an argmax on a function over the list
            var bestAttr:Symbol = argmax(attrs.keySet-label, (x:Symbol) => 
                                         informationGain(x,attrs,examples,label))
            println(levstr+"best node is %s".format(bestAttr))
            // now we produce a new branch, which splits on that node, and recurse down the nodes.
            var branch = new Branch(bestAttr)
            for(v <- attrs(bestAttr)){
                println(levstr+"New node for %s".format(v))
                val subset = examples.filter(x=> x(bestAttr)==v)
                if(subset.size == 0){
                    println(levstr+"Tiny subset!")
                    // zero subset, we replace with a leaf labelled with the most common label in
                    // the examples
                    val m = examples.map(_(label))
                    val mostCommonLabel = m.toSet.map((x:Symbol) => (x,m.count(_==x))).maxBy(_._2)._1
                    branch.add(v,new Leaf(mostCommonLabel))
                } else {
                    println(levstr+"Branch on %s=%s!".format(bestAttr,v))
                    branch.add(v,id3(attrs,subset,label))
                }
            }
            level = level-1
            branch
        }
    }
}

    
object Driver {
    def main(args: Array[String]){
        
        var attrs: Tree.Attributes = Map()
        
        attrs += ('debt -> Set('medium,'high,'low))
        attrs += ('income -> Set('medium,'high,'low))
        attrs += ('job -> Set('permanent,'temporary))
        attrs += ('married -> Set('yes,'no))
        attrs += ('lifestyle -> Set('frugal,'luxury))
        
        attrs += ('risk -> Set('good,'poor))
        
        val examples: List[Tree.Example] = List(
                 Map(
                     'debt -> 'medium,
                     'income -> 'high,
                     'married -> 'yes,
                     'job -> 'permanent,
                     'lifestyle -> 'frugal,
                     'risk -> 'good
                     ),
                 Map(
                     'debt -> 'low,
                     'income -> 'medium,
                     'married -> 'yes,
                     'job -> 'temporary,
                     'lifestyle -> 'luxury,
                     'risk -> 'good
                     ),
                 Map(
                     'debt -> 'low,
                     'income -> 'high,
                     'married -> 'no,
                     'job -> 'temporary,
                     'lifestyle -> 'luxury,
                     'risk -> 'poor
                     ),
                 Map(
                     'debt -> 'high,
                     'income -> 'low,
                     'married -> 'yes,
                     'job -> 'temporary,
                     'lifestyle -> 'frugal,
                     'risk -> 'poor
                     ),
                 Map(
                     'debt -> 'medium,
                     'income -> 'medium,
                     'married -> 'yes,
                     'job -> 'permanent,
                     'lifestyle -> 'luxury,
                     'risk -> 'poor
                     ),
                 Map(
                     'debt -> 'high,
                     'income -> 'medium,
                     'married -> 'no,
                     'job -> 'permanent,
                     'lifestyle -> 'frugal,
                     'risk -> 'good
                     ),
                 Map(
                     'debt -> 'medium,
                     'income -> 'low,
                     'married -> 'no,
                     'job -> 'temporary,
                     'lifestyle -> 'frugal,
                     'risk -> 'poor
                     )
                 )
        // obviously you can't use the label as an attribute, that would be silly!
        val label = 'risk
        Tree.validate(attrs,examples,label)
        println(Tree.id3(attrs,examples,label).getStr(0))
    }
}

