package chapter6

import chapter6.StatefulApi.State
import chapter6.StatefulApi.State._

object CandyDispenser {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  
  case class Machine(locked: Boolean, candies: Int,  coins: Int){
    
    def operate(input: Input) = input match {
      case Coin if(this.locked && this.candies > 0) => this.copy(locked = false, coins = this.coins + 1)
      case Turn if(!this.locked) => this.copy(locked = true, candies = this.candies - 1)
      case _ => this  
    }  
  }

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _))               => s
      case (Coin, Machine(false, _, _))        => s
      case (Turn, Machine(true, _, _))         => s
      case (Coin, Machine(true, candy, coin))  => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }
    
  def createStateChanger = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _))               => s
      case (Coin, Machine(false, _, _))        => s
      case (Turn, Machine(true, _, _))         => s
      case (Coin, Machine(true, candy, coin))  => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }  
    
  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
  
  def wtf(inputs: List[Input]) = {
    val result = inputs.map(i => (modify[Machine] _ compose update)) /// how is "i" used ?????
    result
  }
  
  def wtfBetter(inputs: List[Input]) = {
    val stateChangers = inputs.map(update)
    val stateChanges = stateChangers.map(modify[Machine])
    stateChanges
  }
  
  def wtfBetter2(inputs: List[Input]) = {
    
    inputs.map(i => modify(update(i)))
  }
  
  def simulateMachine2Expilcit(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => { val u = update(i) ; modify[Machine](u) }))
    s <- get
  } yield (s.coins, s.candies)
  
  def simulateMachine2Neat(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(createStateChanger andThen modify))
    s <- get
  } yield (s.coins, s.candies)
  
  def simulateMachine2Explained(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def stateChanges(inputs: List[Input]) = {
      val stateChangers = inputs.map(update)
      val stateChanges = stateChangers.map(modify[Machine])
      stateChanges
    }

    for {
      _ <- sequence(stateChanges(inputs))
      s <- get // getting from where though???
    } yield (s.coins, s.candies)
  }
  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
         
    State(machine => 
      inputs.foldLeft(((machine.candies, machine.coins), machine))((acc, cur) => {
        val m = acc._2 operate cur
        ((m.candies, m.coins), m)
      }))
  }
  
  def sim(inputs: List[Input]): State[Machine, (Int, Int)] = {
 
    State(m => {
      val newM = inputs.foldLeft(m)(_ operate _)
      ((newM.candies, newM.coins), newM)
    })
  }
  
  def sim2(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def opMany(m: Machine) = {
      val newM = inputs.foldLeft(m)(_ operate _)
      ((newM.candies, newM.coins), newM)
    }
    
    State(opMany)
    
  }
  
  def sim3(inputs: List[Input]): State[Machine, List[(Int, Int)]] = {
    
    val stateTransitions = inputs.map(i => 
      State((m: Machine) => {
      val state = m operate i
      ((state.candies, state.coins),state)
    })) 
    
    sequence(stateTransitions)
  }
  
  def sim4(inputs: List[Input]): State[Machine, (Int, Int)] = {
    
    val stateTransitions = inputs.map(i => 
      State((m: Machine) => {
      val state = m operate i
      ((state.candies, state.coins),state)
    })) 
    
    sequence(stateTransitions).flatMap(a => State(m => (a.last, m)))
  }
  
  def sim5(inputs: List[Input]): State[Machine, (Int, Int)] = {
    
    val stateTransitions = inputs.map(i => 
      State((m: Machine) => {
      val state = m operate i
      ((state.candies, state.coins),state)
    })) 
    
    sequence(stateTransitions).map(a => a.last)
  }
  
  def sim6(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val stateTransitions = inputs.map(i => 
      State((m: Machine) => {
      val state = m operate i
      ((state.candies, state.coins),state)
    })) 
    
    for { 
      ts <- sequence(stateTransitions)
    } yield(ts.last)
  }
}