import java.io.{File, PrintWriter}

import scala.collection.{immutable, mutable}
import scala.collection.immutable.{List, Map}
import scala.io.Source


object Allocator {
  val AcceptingTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>");
  var successfulScan: Boolean = true

  val scanner: ILOCScanner = new ILOCScanner
  val parser: ILOCParser = new ILOCParser
  val renamer: ILOCRenamer =  new ILOCRenamer
  val allocator: ILOCAllocator = new ILOCAllocator
  val executor: ILOCExecutor = new ILOCExecutor
  var flag: String = "-p"
  var flagPriority = 0
  var filename = ""
  var fileReceived = false
  var errorCount: Int = 0
  var k: Int = 0
  def main(args: Array[String]): Unit = {
    for(arg <- args) {
      arg match {
        case "-h" =>
          if(flagPriority < 4) {
            flag = "-h"
            flagPriority = 4
          }
        case "-s" =>
          if(flagPriority < 1) {
            flag = "-s"
            flagPriority = 1
          }
        case "-p" =>
          if(flagPriority < 2) {
            flag = "-p"
            flagPriority = 2
          }
        case "-r" =>
          if(flagPriority < 3) {
            flag = "-r"
            flagPriority = 3
          }
        case "-x" =>
          if(flagPriority < 4) {
            flag = "-x"
            flagPriority = 4
          }
        case _=>
          try {
            k = Integer.parseInt(arg)
            allocator.prNumber = k
            flag = "-k"
          } catch {
            case n: NumberFormatException =>
              if(filename.isEmpty) filename = arg else flag = "-h"
          }
      }
    }
    //println("Flag: " + flag)
    flag match {
      case "-x" =>
        val IR = scanAndParse(filename)
        for(op <- IR.filter(_.isDefined)) {
          //println(op.get)
        }
        //println("--------------------------")
        val preAll = rename(IR)
        for(op <- preAll) {
          println(op)
        }
      case "-k" =>
        //println("There are " + k + " registers")
        //println("Rename:")
        val IR = scanAndParse(filename)
        val IR2 = IR.filter(_.isDefined).map(_.get)
        val preAll = rename(IR)
        val postAll = allocate(preAll)

      /*for(op <- preAll) { //Regular Print Rename
          println(op)
        }
        println("\nReallocate: ")*/

        /*for(op <- postAll) {    //Print Alloc
          println(op)
        }*/

        val renameVal = executor.runProg(IR2,true,"cc6o.txt")     //Operation Dissect
        println(renameVal._1)
        println("\nReallocate: ")
        val reAllVal = executor.runProg(postAll,true,"cc6a.txt")
        println(reAllVal._1)
        println("176: " + reAllVal._1.get(176))


        //println("1024: " + reAllVal._1.get(1024))



    }

  }


  def scanAndParse(filename: String): List[Option[Operation]] = {
    var lineNumber: Int = 1
    var fullScan: List[List[Any]] = List.empty
    val input = Source.fromFile(filename)
    for (line <- input.getLines) {
      //parseLine(line,lineNumber)
      val scan: List[Any] = scanner.scanLine(line,lineNumber).reverse
      //print("Line " + lineNumber + " - ")
      //if(scan.contains(-1)) println("Error: " + scan) else println("Success: " + scan)
      lineNumber += 1
      fullScan = fullScan.::(scan.filterNot(l => l == -1 || l == 10))
    }
    fullScan = fullScan.reverse

    input.close()
    lineNumber = 1
    var interRep: List[Option[Operation]] = List.empty
    for(i <- fullScan) {
      interRep = interRep.::(parser.parse(i,lineNumber))
      lineNumber += 1
    }
    interRep = interRep.reverse
    interRep
  }

  def rename(operations: List[Option[Operation]]): List[Operation] = {
    val fullOps = operations.filter(_.isDefined).map(_.get)
    var opList: List[Operation] = List.empty
    for(i <- fullOps.indices.reverse) {
      val no = renamer.nameOp(fullOps(i),i)
      opList = opList.::(no)
      //println(no)
    }
    opList
  }

  def allocate(operations: List[Operation]): List[Operation] = {
    allocator.setPR(k)
    val reg1: mutable.Map[Register, Int] = mutable.Map()
    val reg2: mutable.Map[Register, Int] = mutable.Map()
    val store1: mutable.Map[Int, Int] = mutable.Map();
    val store2: mutable.Map[Int, Int] = mutable.Map();
    var opList: List[Operation] = List.empty
    for(i <- operations.indices) {
      val tempList = allocator.allocate(operations(i),i)
      //executor.execute(operations(i),reg1,store1)
//      println("Old: " + operations(i))
      for(i <- tempList.reverse) {
        //println(i)
        //executor.execute(i,reg2,store2)
      }
  //    println("Original Storage Values: " + store1.filter(_._1 < 32768))
    //  println("New Storage Values: " + store2.filter(_._1 < 32768))
//      require(store1.filter(_._1 < 32768).values.toList.sortBy(a => a).equals(store2.filter(_._1 < 32768).values.toList.sortBy(a => a)))
      //println("Original Register Values: " + reg1 + " " + store1.filterNot(_._1 < 32768) + " Translated: " + store1.filterNot(_._1 < 32768).values.toList.:::(reg1.values.toList).sortBy(a => a))
      //println("New Register Values: " + reg2 + " " + store2.filterNot(_._1 < 32768) + " Translated: " + store2.filterNot(_._1 < 32768).values.toList.:::(reg2.values.toList).sortBy(a => a))
      //require(store1.filterNot(_._1 < 32768).values.toList.:::(reg1.values.toList).sortBy(a => a) == store2.filterNot(_._1 < 32768).values.toList.:::(reg2.values.toList).sortBy(a => a))
      opList = opList.:::(tempList)
    }

    opList.reverse
  }

  class ILOCScanner {
    //val ILOCfunctions: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop");
    val AcceptedTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>")
    //def isValidOperation(word: String): Boolean = ILOCfunctions.contains(word)

    def TokenMap(op: String): Any = {
      op match {
        case "load" => 0
        case "loadI" => 1
        case "store" => 2
        case "add" => 3
        case "sub" => 4
        case "mult" => 5
        case "lshift" => 6
        case "rshift" => 7
        case "output" => 8
        case "nop" => 9
        case "//" => 10
        case "," => 11
        case "=>" => 12
        case " " => 15
        case _=>
          if(ValidRegisterLabel(op, true)) {
            Register(Integer.parseInt(op.substring(1)))
          } else if(ValidConstant(op)) {
            Constant(Integer.parseInt(op))
          } else {
            -1
          }
      }
    }

    def TokenNotation(op: String): Any = {
      op match {
        case "load" | "store" => "MEMOP"
        case "loadI" => "LOADI"
        case "add" | "sub" | "mult" | "lshift" | "rshift" => "ARITHOP"
        case "output" => "OUTPUT"
        case "nop" => "NOP"
        case "//" => "COMMENT"
        case "," => "COMMA"
        case "=>" => "INTO"
        case _=>
          if(ValidRegisterLabel(op, true)) {
            "REG"
          } else if(ValidConstant(op)) {
            "CONST"
          } else {
            -1
          }
      }
    }

    def ValidRegisterLabel(register: String, complete: Boolean): Boolean = {
      if(register.length == 1) {
        if(complete) false else register(0) == 'r'
      } else {
        register.length > 1 && register(0) == 'r' && register.substring(1).filterNot(_.isDigit).isEmpty
      }
    }

    def ValidConstant(constant: String): Boolean = !constant.isEmpty && constant.filterNot(_.isDigit).isEmpty

    def scanLine(line: String, lineNumber: Integer): List[Any] = {
      var index: Integer = 0
      var tokenIndex: Integer = 0
      var errorState: Boolean = false
      var token: String = ""
      var tokenList: List[Any] = List.empty
      var comment: Boolean = false
      var PotentialTokens: mutable.Set[String] = AcceptedTokens

      def c: Char = if(index >= line.length) line.last else line(index)
      while(index < line.length && c.isWhitespace) {
        index += 1
      }
      while(index < line.length && !comment) {
        //println(c + " - Index: " + index + "/" + line.length)
        if(!c.isWhitespace) PotentialTokens = PotentialTokens.filter(t => (tokenIndex < t.length) && (t(tokenIndex) == c))
        if((!ValidRegisterLabel(token + c, false) && !ValidConstant(token + c) && PotentialTokens.isEmpty) || (c.isWhitespace && !token.isEmpty)) {
          val tokenValue = TokenMap(token)
          if(tokenValue != -1) {
            //println("Valid")
            tokenList = tokenList.::(tokenValue)
            if(flag =="-s" && token != "//") println((TokenNotation(token),"\"" + token + "\""))
            if(token == "//") comment = true
            token = ""
            tokenIndex = 0
            PotentialTokens = AcceptedTokens
          }
          else {
            //println("InValid")
            PotentialTokens = AcceptedTokens
            if(c.isWhitespace && token.isEmpty) {
              index += 1
              token = ""
              tokenIndex = 0
            } else {
              //println("Invalid")
              token += c
              //tokenList = tokenList.::(tokenValue)
              if(flag == "-p" || flag == "-r") System.err.println("SCANNING ERROR [" + lineNumber + "]: " + token + " is not a valid word")
              errorCount += 1
              tokenList = tokenList.::(tokenValue)
              errorState = true
              token = ""
              tokenIndex = 0
              index += 1

            }

          }
        }
        else {
          //println("Valid 2")
          if(!c.isWhitespace) {token += c; tokenIndex += 1}
          index += 1
          //println("New Token: " + token)
        }
      }

      if((TokenMap(token) != -1)) {
        tokenList = tokenList.::(TokenMap(token))
      } else if(!token.isEmpty) {
        System.err.println("SCANNING ERROR [" + lineNumber + "]: " + token + " is not a valid word")
        errorCount += 1
      }
      tokenList
    }
  }

  class ILOCParser {
    def parse(line: List[Any], lineNumber: Integer): Option[Operation] = {
      if(line.isEmpty) return Option.empty
      val head = line.head
      val tail = line.tail
      head match {
        case 0 | 2 => validMemOp(head.asInstanceOf[Int], tail, lineNumber)
        case 1 => validLoadI(tail, lineNumber) //Option(validLoadI(tail))
        case 3 | 4 | 5 | 6 | 7  => validArithOp(head.asInstanceOf[Int] ,tail,lineNumber) //Option(validArithOp(tail))
        case 8 => validOutput(tail, lineNumber)//Option(validOutput(tail))
        case 9 => validNop(tail,lineNumber) //Option(validNop(tail))
        case _=>
          if(flag == "-p" || flag == "-r") {
            System.err.println("PARSING ERROR [" + lineNumber + "]: Operation begins with invalid OpCode")
            errorCount += 1
          }
          Option.empty
      }
    }

    def validMemOp(opCode: Int, tokens: List[Any], lineNumber: Integer): Option[MemOp] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") { System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Source Register in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 2 || tokens(1) != 12) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing \'=>\' in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 3 || !tokens(2).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Target Register in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 3) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      }
      else {
        opCode match {
          case 0 => Option(new Load(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register]))
          case 2 => Option(new Store(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register]))
        }
      }
    }

    def validLoadI(tokens: List[Any], lineNumber: Int): Option[LoadI] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Constant]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Constant in LoadI Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 2 || tokens(1) != 12) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing \'=>\' in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 3 || !tokens(2).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Target Register in LoadI Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 3) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      } else {
        Option(new LoadI(tokens.head.asInstanceOf[Constant],tokens(2).asInstanceOf[Register]))
      }
    }

    def validArithOp(opCode: Int, tokens: List[Any], lineNumber: Int): Option[ArithOp] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing First Source Register in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 2 || tokens(1) != 11) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Comma Separation in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 3 || !tokens(2).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Second Source Register in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 4 || tokens(3) != 12) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing \'=>\' in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 5 || !tokens(4).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Target Register in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 5) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      } else {
        opCode match {
          case 3 => Option(new Add(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 4 => Option(new Sub(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 5 => Option(new Mult(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 6 => Option(new LShift(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 7 => Option(new RShift(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
        }
      }
    }

    def validOutput(tokens: List[Any], lineNumber: Int): Option[Output] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Constant]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Constant for Output"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 1) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      }else {
        Option(new Output(tokens.head.asInstanceOf[Constant]))
      }
    }

    def validNop(tokens: List[Any], lineNumber: Int): Option[Nop] = {
      if(tokens.nonEmpty) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      } else {
        Option(new Nop())
      }
    }
  }

  class ILOCRenamer {
    val RegisterUseMap: mutable.Map[Register, Int] = mutable.Map();
    val RegisterVrMap: mutable.Map[Register, Register] = mutable.Map()
    private var vrNumber = 0

    def getVR(opNumber: Int): Register = {
      vrNumber += 1
      val newReg = Register(vrNumber-1)
      newReg
    }

    def nameOp(op: Operation, opNumber: Int): Operation = {
      require(op.regType == 1)
      op match {
        case load: Load =>
          if(RegisterVrMap.get(load.r2).isEmpty) {
            RegisterVrMap.update(load.r2, getVR(opNumber))
          }
          val loadValue = RegisterVrMap(load.r2)
          RegisterVrMap.remove(load.r2)

          if(RegisterVrMap.get(load.r1).isEmpty) {
            RegisterVrMap.update(load.r1, getVR(opNumber))
          }
          val newLoad = new Load(RegisterVrMap(load.r1),loadValue)
          newLoad.r1NU = RegisterUseMap.get(newLoad.r1)
          newLoad.r2NU = RegisterUseMap.get(newLoad.r2)
          RegisterUseMap.update(newLoad.r1,opNumber)
          RegisterUseMap.update(newLoad.r2,opNumber)
          newLoad
        case store: Store =>
          if(RegisterVrMap.get(store.r1).isEmpty) {
            RegisterVrMap.update(store.r1, getVR(opNumber))
          }
          if(RegisterVrMap.get(store.r2).isEmpty) {
            RegisterVrMap.update(store.r2, getVR(opNumber))
          }
          val newStore = new Store(RegisterVrMap(store.r1),RegisterVrMap(store.r2))
          newStore.r1NU = RegisterUseMap.get(newStore.r1)
          newStore.r2NU = RegisterUseMap.get(newStore.r2)
          RegisterUseMap.update(newStore.r1,opNumber)
          RegisterUseMap.update(newStore.r2,opNumber)
          newStore
        case loadI: LoadI => {
          if(RegisterVrMap.get(loadI.r).isEmpty) {
            RegisterVrMap.update(loadI.r, getVR(opNumber))
          }
          val newLoadI = new LoadI(loadI.x, RegisterVrMap(loadI.r))
          RegisterVrMap.remove(loadI.r)
          newLoadI.rNU = RegisterUseMap.get(newLoadI.r)
          RegisterUseMap.update(newLoadI.r, opNumber)
          newLoadI
        }
        case arith: ArithOp =>
          if(RegisterVrMap.get(arith.r3).isEmpty) {
            RegisterVrMap.update(arith.r3, getVR(opNumber))
          }
          val newDef  = RegisterVrMap(arith.r3)
          RegisterVrMap.remove(arith.r3)
          if(RegisterVrMap.get(arith.r1).isEmpty) {
            RegisterVrMap.update(arith.r1, getVR(opNumber))
          }
          if(RegisterVrMap.get(arith.r2).isEmpty) {
            RegisterVrMap.update(arith.r2, getVR(opNumber))
          }


          val newArith = arith match {
            case add: Add => new Add(RegisterVrMap(add.r1),RegisterVrMap(add.r2),newDef)
            case sub: Sub => new Sub(RegisterVrMap(sub.r1),RegisterVrMap(sub.r2),newDef)
            case mult: Mult => new Mult(RegisterVrMap(mult.r1),RegisterVrMap(mult.r2),newDef)
            case lshift: LShift => new LShift(RegisterVrMap(lshift.r1),RegisterVrMap(lshift.r2),newDef)
            case rshift: RShift => new RShift(RegisterVrMap(rshift.r1),RegisterVrMap(rshift.r2),newDef)
          }
          newArith.r1NU = RegisterUseMap.get(RegisterVrMap(arith.r1))
          newArith.r2NU = RegisterUseMap.get(RegisterVrMap(arith.r2))
          newArith.r3NU = RegisterUseMap.get(newDef)
          RegisterUseMap.update(RegisterVrMap(arith.r1),opNumber)
          RegisterUseMap.update(RegisterVrMap(arith.r2),opNumber)
          RegisterUseMap.update(newDef,opNumber)

          newArith
        case _=> op
      }
    }
  }

  class ILOCAllocator {

    var prNumber: Int = 0
    val PrUse: mutable.Map[Register, Int] = mutable.Map()
    val freePRs: mutable.Map[Int, Register] = mutable.Map()
    val PRToVR: mutable.Map[Register,Register] = mutable.Map()
    val VRToPR: mutable.Map[Register,Register] = mutable.Map()
    val PRNU: mutable.Map[Register, Option[Int]] = mutable.Map()
    val VRNU: mutable.Map[Register, Option[Int]] = mutable.Map()
    var spillRegister: Register = _
    val VRToSpill: mutable.Map[Register, Int] = mutable.Map()

    var spillOp1: Option[Operation] = Option.empty
    var spillOp2: Option[Operation] = Option.empty
    var restoreOp1: Option[Operation] = Option.empty
    var restoreOp2: Option[Operation] = Option.empty
    var mainOp: Option[Operation] = Option.empty
    var opList: List[Operation] = List.empty
    def setPR(registers: Int): Unit = {
      for(i <- 0 until registers-1) {
        freePRs.update(i,Register(i))
      }
      //println(freePRs)
      spillRegister = Register(registers-1)
    }

    def getPR(): Register = {
      require(VRNU.forall(x => VRToPR.contains(x._1)))
      if(freePRs.isEmpty) {


        return spillPR()
      }
      var pri = 0
      var pr = freePRs.get(pri)
      while(pr.isEmpty && pri < k) {
        pri += 1
        pr = freePRs.get(pri)
      }
      freePRs.remove(pri)
      pr.get
    } ensuring(VRNU.forall(x => VRToPR.contains(x._1)))

    def restorePR(register: Register): Register = {
      require(VRNU.forall(x => VRToPR.contains(x._1)))
      //println("Restoring " + register)
      restoreOp1 = Option(new LoadI(Constant(VRToSpill(register)),spillRegister))
      val resOp1 = new LoadI(Constant(VRToSpill(register)),spillRegister)
      VRToSpill.remove(register)
      //println("Free PRs: " + freePRs)
      //println(VRToPR)

      val newPR = getPR()
      //println("PR: " + newPR)
      VRToPR.update(register,newPR)
      PRToVR.update(newPR,register)
      restoreOp2 = Option(new Load(spillRegister,newPR))
      //opList = opList.::()
      val resOp2 = new Load(spillRegister,newPR)
      opList = opList.::(resOp1)
      opList = opList.::(resOp2)

      newPR
    } ensuring(VRNU.forall(x => VRToPR.contains(x._1)))

    def freePR(register: Register): Unit = {
      require(VRNU.forall(x => VRToPR.contains(x._1)))
        require(VRNU(PRToVR(register)).isEmpty)
        VRToPR.remove(PRToVR(register))
        VRNU.remove(PRToVR(register))
        PRToVR.remove(register)
        freePRs.update(register.label,register)
    } ensuring(VRNU.forall(x => VRToPR.contains(x._1)))

    def spillPR(): Register = {
      require(VRNU.forall(x => VRToPR.contains(x._1)))
      var spillLoc: Int = 32768
      while(VRToSpill.values.toSet.contains(spillLoc)) {
        spillLoc += 4
      }
      //println(VRNU)
      //println(VRToPR)
      val vrSpill: Register = if(VRNU.exists(p => p._2.isEmpty)) VRNU.filter(p => p._2.isEmpty).toList.head._1 else VRNU.maxBy(a => a._2)._1

      val prSpill: Register = VRToPR(vrSpill)

      //println("Spilling " + vrSpill)
      VRToPR.remove(vrSpill)
      VRNU.remove(vrSpill)
      VRToSpill.update(vrSpill,spillLoc)
      spillOp1 = Option(new LoadI(Constant(spillLoc),spillRegister))
      spillOp2 = Option(new Store(prSpill,spillRegister))

      opList = opList.::(new LoadI(Constant(spillLoc),spillRegister))
      opList = opList.::(new Store(prSpill,spillRegister))
      prSpill
    } ensuring(VRNU.forall(x => VRToPR.contains(x._1)))

    def allocate(op: Operation, opNumber: Int): List[Operation] = {
      spillOp1 = Option.empty
      spillOp2 = Option.empty
      restoreOp1 = Option.empty
      restoreOp2 = Option.empty
      mainOp = Option.empty
      opList = List.empty
      require(VRNU.forall(x => VRToPR.contains(x._1) || VRToSpill.contains(x._1)))
      op match {
        case load: Load =>
          val pr1: Register = if (VRToPR.get(load.r1).isEmpty) restorePR(load.r1) else VRToPR(load.r1)
          if(VRToPR.get(load.r1).isEmpty) {
            VRToPR.update(load.r1,pr1)
            PRToVR.update(pr1,load.r1)
          }

          val pr2 = getPR()
          PRNU.update(pr1, load.r1NU)
          VRNU.update(load.r1, load.r1NU)
          if(load.r1NU.isEmpty) {
            freePR(pr1)
          }
          VRToPR.update(load.r2,pr2)
          PRToVR.update(pr2,load.r2)
          PRNU.update(pr2, load.r2NU)
          VRNU.update(load.r2, load.r2NU)
          mainOp = Option(new Load(pr1,pr2))
        case store: Store=>
          //println("Store Registers: " + store.r1 +", " + store.r2)
          //println(VRNU)
          //println(VRToPR)
          //println(VRToSpill)


          val pr1: Register = if (VRToPR.get(store.r1).isEmpty) restorePR(store.r1) else VRToPR(store.r1)
          //println("Pr1:" + pr1)
          if(VRToPR.get(store.r1).isEmpty) {
            VRToPR.update(store.r1,pr1)
            PRToVR.update(pr1,store.r1)
          }

          val pr2: Register = if (VRToPR.get(store.r2).isEmpty) restorePR(store.r2) else VRToPR(store.r2)
          //println("Pr2: " + pr2)
          if(VRToPR.get(store.r2).isEmpty) {
            VRToPR.update(store.r2,pr2)
            PRToVR.update(pr2,store.r2)
          }
          PRNU.update(pr1, store.r1NU)
          VRNU.update(store.r1, store.r1NU)
          PRNU.update(pr2, store.r2NU)
          VRNU.update(store.r2, store.r2NU)

          if(store.r1NU.isEmpty) {
            freePR(pr1)
          }
          if(store.r2NU.isEmpty) {
            freePR(pr2)
          }
          mainOp = Option(new Store(pr1,pr2))
        case loadI: LoadI=>
          var pr: Register = null
          if(VRToPR.get(loadI.r).isEmpty) {
            pr = getPR()
            VRToPR.update(loadI.r,pr)
            PRToVR.update(pr,loadI.r)
            PRNU.update(pr, loadI.rNU)
            VRNU.update(loadI.r, loadI.rNU)
            //println(VRToPR)
            //println(PRToVR)
            //println(PRToVR.get(Register(2)))
          }
          mainOp = Option(new LoadI(loadI.x,pr))
        case arith: ArithOp =>
          //println(VRToSpill)

          val pr1: Register = if (VRToPR.get(arith.r1).isEmpty) restorePR(arith.r1) else VRToPR(arith.r1)
          //println("PR1: " + pr1)
          val pr2: Register = if (VRToPR.get(arith.r2).isEmpty) restorePR(arith.r2) else VRToPR(arith.r2)



          val pr3 = getPR()

          VRToPR.update(arith.r3,pr3)
          PRToVR.update(pr3,arith.r3)
          //println("VR to PR: " + VRToPR)
          //println("Free PRs: "  + freePRs)
          PRNU.update(pr1, arith.r1NU)
          VRNU.update(arith.r1, arith.r1NU)
          PRNU.update(pr2, arith.r2NU)
          VRNU.update(arith.r2, arith.r2NU)
          if(arith.r1NU.isEmpty) {
            freePR(pr1)
          }

          if(arith.r2NU.isEmpty && pr1 != pr2) {
            freePR(pr2)
          }
          PRNU.update(pr3, arith.r3NU)
          VRNU.update(arith.r3, arith.r3NU)
          //require(VRNU.forall(x => VRToPR.contains(x._1)))
          mainOp = arith match {
            case _: Add => Option(new Add(pr1,pr2,pr3))
            case _: Sub => Option(new Sub(pr1,pr2,pr3))
            case _: Mult => Option(new Mult(pr1,pr2,pr3))
            case _: LShift => Option(new LShift(pr1,pr2,pr3))
            case _: RShift => Option(new RShift(pr1,pr2,pr3))
          }
        case _=> mainOp = Option(op)
      }
      val l = List(spillOp1,spillOp2,restoreOp1,restoreOp2,mainOp).filter(o => o.isDefined).map(o => o.get)
      //l.reverse
      opList.::(mainOp.get)


    } ensuring(VRNU.forall(x => VRToPR.contains(x._1)))
  }

  class ILOCExecutor {
    val storage: mutable.Map[Int, Int] = mutable.Map()
    val registers: mutable.Map[Register, Int] = mutable.Map()
    var spill = false
    var restore = false
    def reset(): Unit = {
      storage.clear()
      registers.clear()
    }

    def runProg(program: List[Operation], print: Boolean, filename: String): (mutable.Map[Int, Int], mutable.Map[Register, Int]) = {
      reset()
      val pw = new PrintWriter(new File(filename ))


      var opCount = 0
      for(op <- program.indices) {
        execute(program(op),registers,storage)
        if(print) {
          program(op) match {
            case loadI: LoadI if loadI.x.value >= 32768 && op+1 < program.length && program(op+1).isInstanceOf[Store] =>
              pw.write("SPILL: " + program(op) + "\nSPILL: Storage: " + storage.toList.sortBy(s => s._2) + "\nSPILL: Registers: " + registers.filterNot(r => registers(r._1) >= 32768).toList.sortBy(r => r._2))
              spill = true
            case loadI: LoadI if loadI.x.value >= 32768 && op+1 < program.length && program(op+1).isInstanceOf[Load] =>
              pw.write("RESTORE: " + program(op) + "\nRESTORE: Storage: " + storage.toList.sortBy(s => s._2) + "\nRESTORE: Registers: " + registers.filterNot(r => registers(r._1) >= 32768).toList.sortBy(r => r._2))
              restore = true
            case _ =>
              if(spill) {
                pw.write("SPILL: " + program(op) + "\nSPILL: Storage: " + storage + "\nSPILL: Registers: " + registers.filterNot(r => registers(r._1) >= 32768).toList.sortBy(r => r._1.label))
                spill = false
              } else if(restore) {
                pw.write("RESTORE: " + program(op) + "\nRESTORE: Storage: " + storage.toList.sortBy(s => s._2) + "\nRESTORE: Registers: " + registers.filterNot(r => registers(r._1) >= 32768).toList.sortBy(r => r._2))
                restore = false
              } else {
                pw.write("OP " + opCount + ": " + program(op) + "\nOP " + opCount + ": Storage: " + storage.toList.sortBy(s => s._2) + "\nOP " + opCount + ": Registers: " + registers.filterNot(r => registers(r._1) >= 32768).toList.sortBy(r => r._2))
                opCount += 1
              }
          }
        }
      }
      pw.close()
      (storage,registers)
    }

    def compareProgs(progs: (List[Operation], List[Operation])): Unit = {
      require(progs._1.length <= progs._2.length)
      var opNumber: Int = 0
      var opIndex1: Int = 0
      var opIndex2: Int = 0
      var spill = false
      var restore = false
      val program1 = progs._1
      val program2 = progs._2
      val stor1: mutable.Map[Int, Int] = mutable.Map()
      val reg1: mutable.Map[Register, Int] = mutable.Map()
      val stor2: mutable.Map[Int, Int] = mutable.Map()
      val reg2: mutable.Map[Register, Int] = mutable.Map()
      while(opIndex1 < program1.length) {
        program2(opIndex2) match {
          case loadI: LoadI if loadI.x.value >= 32768 && opIndex2+1 < program2.length && program2(opIndex2+1).isInstanceOf[Store] =>
            println("SPILL: " + program2(opIndex2) + "\nSPILL: Storage: " + stor2 + "\nSPILL: Registers: " + reg2.filterNot(r => reg2(r._1) >= 32768).toList.sortBy(r => r._1.label))
            spill = true
            execute(program2(opIndex2),reg2,stor2)
            opIndex2 += 1
          case loadI: LoadI if loadI.x.value >= 32768 && opIndex2+1 < program2.length && program2(opIndex2+1).isInstanceOf[Load] =>
            println("RESTORE: " + program2(opIndex2) + "\nRESTORE: Storage: " + stor2 + "\nRESTORE: Registers: " + reg2.filterNot(r => reg2(r._1) >= 32768).toList.sortBy(r => r._1.label))
            restore = true
            execute(program2(opIndex2),reg2,stor2)
            opIndex2 += 1
          case _ =>
            if(spill) {
              println("SPILL: " + program2(opIndex2) + "\nSPILL: Storage: " + stor2 + "\nSPILL: Registers: " + reg2.filterNot(r => reg2(r._1) >= 32768).toList.sortBy(r => r._1.label))
              spill = false
              execute(program2(opIndex2),reg2,stor2)
              opIndex2 += 1
            } else if(restore) {
              println("RESTORE: " + program2(opIndex2) + "\nRESTORE: Storage: " + stor2 + "\nRESTORE: Registers: " + reg2.filterNot(r => reg2(r._1) >= 32768).toList.sortBy(r => r._1.label))
              restore = false
              execute(program2(opIndex2),reg2,stor2)
              opIndex2 += 1
            } else {
              //println("OP " + opNumber + ": " + program(op) + "\nOP " + opCount + ": Storage: " + storage + "\nOP " + opCount + ": Registers: " + registers.filterNot(r => registers(r._1) >= 32768).toList.sortBy(r => r._1.label))
              println("OP " + opNumber + ": " + program1(opIndex1) + " --- " + program2(opIndex2))
              println("OP " + opNumber + ": RegValues: " + storage)
              //require
              execute(program1(opIndex1),reg1,stor1)
              opIndex1 += 1
              execute(program2(opIndex2),reg2,stor2)
              opIndex2 += 1
              opNumber += 1
            }
        }
      }
    }

/*    def execute(op: Operation): Unit = {
      op match {
        case loadI: LoadI => if(loadI.x.value < 32768) registers.update(loadI.r,loadI.x.value) else registers.update(loadI.r,storage.getOrElse(loadI.x.value,loadI.x.value))
        case load: Load => registers.update(load.r2,registers(load.r1))
        case store: Store => storage.update(registers(store.r2),registers(store.r1))
        case add: Add => registers.update(add.r3,registers(add.r1) + registers(add.r2))
        case sub: Sub => registers.update(sub.r3,registers(sub.r1) - registers(sub.r2))
        case mult: Mult => registers.update(mult.r3,registers(mult.r1) * registers(mult.r2))
        case lshift: LShift => registers.update(lshift.r3,registers(lshift.r1) << registers(lshift.r2))
        case rshift: RShift => registers.update(rshift.r3,registers(rshift.r1) >> registers(rshift.r2))
        case output: Output => println(output.c)
        case nop: Nop => None
      }
    }*/
def execute(op: Operation, registers: mutable.Map[Register, Int], storage: mutable.Map[Int, Int]): Unit = {
  op match {
    case loadI: LoadI => registers.update(loadI.r,loadI.x.value)//if(loadI.x.value < 32768) registers.update(loadI.r,loadI.x.value) else registers.update(loadI.r,storage.getOrElse(loadI.x.value,loadI.x.value))
    case load: Load => registers.update(load.r2,storage.getOrElse(registers(load.r1),0))
    case store: Store => storage.update(registers(store.r2),registers(store.r1))
    case add: Add => registers.update(add.r3,registers(add.r1) + registers(add.r2))
    case sub: Sub => registers.update(sub.r3,registers(sub.r1) - registers(sub.r2))
    case mult: Mult => registers.update(mult.r3,registers(mult.r1) * registers(mult.r2))
    case lshift: LShift => registers.update(lshift.r3,registers(lshift.r1) << registers(lshift.r2))
    case rshift: RShift => registers.update(rshift.r3,registers(rshift.r1) >> registers(rshift.r2))
    case output: Output => println(output.c)
    case nop: Nop => None
  }
}
  }

  class Load(r1: Register, r2: Register) extends MemOp(r1: Register, r2: Register) {

    override def execute(): Unit = ???

    override def toString: String = "load " + r1 + " => " + r2
    //override def toString: String = "load " + r1 + " " + r1NU + " => " + r2 + " " + r2NU

  }

  class Store(r1: Register, r2: Register) extends MemOp(r1: Register, r2: Register) {

    override def execute(): Unit = ???
    override def toString: String = "store " + r1 + " => " + r2
    //override def toString: String = "store " + r1 + " " + r1NU + " => " + r2 + " " + r2NU

  }

  abstract class MemOp(var r1: Register, var r2: Register) extends Operation {
    var r1NU: Option[Int] = Option.empty
    var r1VR: Option[Int] = Option.empty
    var r2NU: Option[Int] = Option.empty
    var r2VR: Option[Int] = Option.empty
  }

  class LoadI(var x: Constant, var r: Register) extends Operation {
    var rNU: Option[Int] = Option.empty
    var rVR: Option[Int] = Option.empty
    override def execute(): Unit = ???

    override def toString: String = "loadI " + x + " => " + r
    //override def toString: String =  "loadI " + x + " => " + r + " " + rNU

  }

  class Add(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {

    override def execute(): Unit = ???

    override def toString: String = "add " + r1 + ", " + r2 + " => " + r3
    //override def toString: String =  "add " + r1 + " " + r1NU + ", " + r2 + " " + r2NU + ", => " + r3 + " " + r3NU
  }

  class Sub(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???

    override def toString: String = "sub " + r1 + ", " + r2 + " => " + r3
    //override def toString: String =  "sub " + r1 +  r1NU + ", " + r2  + r2NU + ", => " + r3
  }

  class Mult(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???

    override def toString: String = "mult " + r1 + ", " + r2 + " => " + r3

    //override def toString: String =  "mult " + r1 + " " + r1NU + ", " + r2 + " " + r2NU + ", => " + r3 + " " + r3NU
  }

  class LShift(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???

    override def toString: String = "lshift " + r1 + ", " + r2 + " => " + r3
    //override def toString: String =  "lshift " + r1 + " " + r1NU + ", " + r2 + " " + r2NU + ", => " + r3
  }

  class RShift(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???

    override def toString: String = "rshift " + r1 + ", " + r2 + " => " + r3
    //override def toString: String =  "rshift " + r1 + r1NU + ", " + r2 + r2NU + ", => " + r3
  }

  abstract class ArithOp(var r1: Register, var r2: Register, var r3: Register) extends Operation {

    var r1NU: Option[Int] = Option.empty
    var r1VR: Option[Int] = Option.empty
    var r2NU: Option[Int] = Option.empty
    var r2VR: Option[Int] = Option.empty
    var r3NU: Option[Int] = Option.empty
    var r3VR: Option[Int] = Option.empty
  }

  class Output(val c: Constant) extends Operation {
    override def execute(): Unit = ???

    override def toString: String = "output " + c
  }

  class Nop() extends Operation {
    override def execute(): Unit = ???

    override def toString: String = "nop"
  }

  abstract class Operation {
    var regType: Int = 1

    def execute(): Unit
  }


  case class Register(label: Int) {

    override def equals(obj: Any): Boolean = obj.isInstanceOf[Register] && (label == obj.asInstanceOf[Register].label)

    override def toString: String = "r" + label

  }



  case class Constant(value: Int) {
    override def toString: String = value.toString
  }

}

