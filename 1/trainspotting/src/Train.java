import TSim.*;
import java.util.concurrent.*;

public class Train extends Thread 
{
  // Instance Variables
  private TSimInterface tsim;
  private int trainID;
  private int trainSpeed;
  private int simSpeed;

  // Class variables
  private final static Semaphore s = new Semaphore(1);

  public Train(int tID, int tSpeed, int sSpeed) {
    this.trainID = tID;
    this.trainSpeed = tSpeed;
    this.simSpeed = sSpeed;
    tsim = TSimInterface.getInstance();
	}

  public void run()
  {
		try {
      tsim.setSpeed(this.trainID, this.trainSpeed);
    } 
    catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
    
    // event loop
    while (true) {
      try {
        update(); 
      }
      catch (Exception e) {
        e.getMessage();
      }
    }  
  }

  /* update status */
  private void update() throws CommandException, InterruptedException {
      SensorEvent sensor = tsim.getSensor(this.trainID);
      System.out.println(sensor.toString());
  }
}
