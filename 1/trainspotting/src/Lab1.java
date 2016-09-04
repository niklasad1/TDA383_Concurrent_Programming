import TSim.*;

public class Lab1 {

  public Lab1(Integer speed1, Integer speed2) {
    // I assume only one thread is created
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      tsi.setSpeed(1,speed1);
      // add second train
      tsi.setSpeed(2,speed2);
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }
}
