import TSim.*;

public class Lab1 {

  public Lab1(Integer speed1, Integer speed2) {
		/* TSimInterface tsi = TSimInterface.getInstance(); */

    // create two objects that execute in individual thread
    Train t1 = new Train(1, speed1, -1);
    Train t2 = new Train(2, speed2, -2);
    t1.start();
    t2.start();


    /* try { */
    /*   tsi.setSpeed(1,speed1); */
    /*   // add second train */
    /*   tsi.setSpeed(2,speed2); */
    /* } */
    /* catch (CommandException e) { */
    /*   e.printStackTrace();    // or only e.getMessage() for the error */
    /*   System.exit(1); */
    /* } */
  }
}
