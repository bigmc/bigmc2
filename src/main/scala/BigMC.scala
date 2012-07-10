
package org.bigraph.bigmc

object BigMC extends App {
    def printUsage = {
        System.err.println("Usage: bigmc2 <filename>")
    }

    def main(args : Array[String]) = {
        if(args.length == 0) {
            printUsage
            System.exit(1)
        }
    }
}



