import TSim.*;
import java.util.concurrent.*;

public class Train extends Thread 
{
  private TSimInterface tsim;
  private int trainID;
  private int trainSpeed;
  private int simSpeed;

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
    catch (CommandException e) {
			e.printStackTrace();
			System.exit(1);
		}    // set speed
  }


}
