
package org.bigraph.bigmc

object BigMCOpts {
    var graphOutput : String = ""
    var maxSteps : Int = 1000
    var printDiscovered : Boolean = false
    var localCheck : Boolean = false
    var reportFrequency : Int = 500
    var verbose : Boolean = false
    var filename : String = ""
    var stochastic : Boolean = false
}

object BigMC extends App {
    def usage = System.err.println("""    
Usage: bigmc2 [options] <filename>

  Options:
    -G file     Output the reaction graph to a dot file.
    -h --help   Display this help and exit.
    -l          Local check mode - do not build the reaction graph.
    -m x        Specify x maximum steps of graph unfolding (default: 1000)
    -p          Print new states as they are discovered.
    -r x        Output statistics and graphs every x steps (default: 500)
    -S          Enable stochastic simulation
    -v          Print version information and exit.
    -V          Print verbose output while running.""")

    def version = println("BigMC v2.0.0\nCopyright (c) 2012 Gian Perrone <gdpe at itu dot dk>")

    def parseOpts(args : List[String]) : Unit = args match {
        case ("-G"::f::t) => {
            BigMCOpts.graphOutput = f
            parseOpts(t)
        }
        case (("-h" | "--help")::t) => {
            usage
            System.exit(0)
        }
        case ("-m"::x::t) => {
            BigMCOpts.maxSteps = x.toInt
            parseOpts(t)
        }
        case ("-p"::t) => {
            BigMCOpts.printDiscovered = true
            parseOpts(t)
        }
        case ("-l"::t) => {
            BigMCOpts.localCheck = true
            parseOpts(t)
        }
        case ("-r"::x::t) => {
            BigMCOpts.reportFrequency = x.toInt
            parseOpts(t)
        }
        case ("-S"::t) => {
            BigMCOpts.stochastic = true
            parseOpts(t)
        }
        case ("-v"::t) => {
            version
            System.exit(0)
        }
        case ("-V"::t) => {
            BigMCOpts.verbose = true
        }
        case n::Nil => {
            BigMCOpts.filename = n
        }
        case _ => {
            usage
            System.exit(1)
        }
    }

    override def main(args : Array[String]) = {
        if(args.length == 0) {
            usage
            System.exit(1)
        } else {
            parseOpts(args.toList)
        }
    }
}



