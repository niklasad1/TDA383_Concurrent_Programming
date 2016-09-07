import TSim.*;
import java.util.concurrent.*;
import java.awt.Point;
import java.util.HashMap;

public class Train extends Thread 
{
  // Instance Variables
  private TSimInterface tsim;
  private int trainID;
  private int trainSpeed;
  private int direction; 
  
  // Class variables
  private final static Semaphore s = new Semaphore(1);
  
  private final static Point[] switches = new Point[] {
	  new Point(17, 7),
	  new Point(15, 9),
	  new Point(4, 9),
	  new Point(3, 11)
  };
  
  
  private final HashMap<SensorEvent, Point> switchSensor = new HashMap<SensorEvent, Point>() {{
    put(new SensorEvent(trainID, 16, 7, SensorEvent.INACTIVE), switches[0]);
    put(new SensorEvent(trainID, 18, 7, SensorEvent.INACTIVE), switches[0]);
    put(new SensorEvent(trainID, 16, 10, SensorEvent.INACTIVE), switches[1]);
    put(new SensorEvent(trainID, 14, 9, SensorEvent.INACTIVE), switches[1]);
    put(new SensorEvent(trainID, 5, 9, SensorEvent.INACTIVE), switches[2]);
    put(new SensorEvent(trainID, 3, 9, SensorEvent.INACTIVE), switches[2]);
    put(new SensorEvent(trainID, 2, 11, SensorEvent.INACTIVE), switches[3]);
    put(new SensorEvent(trainID, 4, 11, SensorEvent.INACTIVE), switches[3]);
  }};




  // METHODS
  
  public Train(int tID, int tSpeed, int tDir) {
    this.trainID = tID;
    this.trainSpeed = tSpeed;
    this.direction = tDir;
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

 
  // compare two sensor events based on position and trainID disregarded
  private boolean sensorEventEquals(SensorEvent e1, SensorEvent e2) {
	  if (e1.getXpos() == e2.getXpos() && e1.getYpos() == e2.getYpos()) return true;
	  else return false;
  }
  
  
  /* update status */
  private void update() throws CommandException, InterruptedException {
    if (this.direction != 1 && this.direction != 2)
    {
      tsim.setSpeed(this.trainID, 0);
      System.out.println("DIRECTION: " + this.direction);
    }

    SensorEvent sensor = tsim.getSensor(this.trainID);
    
    if (sensor.getStatus() == SensorEvent.ACTIVE) {
      
      for (SensorEvent s : switchSensor.keySet()) {
        System.out.println("key: " + s + " value: " + switchSensor.get(s));
        
        if (sensorEventEquals(s, sensor)) {
          Point p = switchSensor.get(s);
          tsim.setSwitch((int)p.getX(), (int)p.getY(), this.direction); 
          break;
        } 
      
      }
    }
  }
}
