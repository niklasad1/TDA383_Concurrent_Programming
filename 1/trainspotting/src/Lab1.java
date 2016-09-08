import TSim.*;

public class Lab1 {

  public Lab1(Integer speed1, Integer speed2) {
		/* TSimInterface tsi = TSimInterface.getInstance(); */

    // create two objects that execute in individual thread
    Train t1 = new Train(1, speed1, 2);
    /* Train t2 = new Train(2, speed2, 1); */ 
    // start threads 
    t1.start();
    // t2.start();
    
    // wait for threads to finish but they will never finish
    try {
      t1.join();
      // t2.join();
    }
    catch (InterruptedException e) {
      e.getMessage();
    } 
  }
}
