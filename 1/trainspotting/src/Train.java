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
  public static final int LEFT = 0x01;
	public static final int RIGHT = 0x02;
  
  
  private final static Point[] switches = new Point[] {
	  new Point(17, 7),
	  new Point(15, 9),
	  new Point(4, 9),
	  new Point(3, 11)
  };
  
  
  /* private final HashMap<SensorEvent, Point> switchSensor = new HashMap<SensorEvent, Point>() {{ */
  /*   put(new SensorEvent(trainID, 16, 7, SensorEvent.INACTIVE), switches[0]); */
  /*   put(new SensorEvent(trainID, 18, 7, SensorEvent.INACTIVE), switches[0]); */
  /*   put(new SensorEvent(trainID, 16, 10, SensorEvent.INACTIVE), switches[1]); */
  /*   put(new SensorEvent(trainID, 14, 9, SensorEvent.INACTIVE), switches[1]); */
  /*   put(new SensorEvent(trainID, 5, 9, SensorEvent.INACTIVE), switches[2]); */
  /*   put(new SensorEvent(trainID, 3, 9, SensorEvent.INACTIVE), switches[2]); */
  /*   put(new SensorEvent(trainID, 2, 11, SensorEvent.INACTIVE), switches[3]); */
  /*   put(new SensorEvent(trainID, 4, 11, SensorEvent.INACTIVE), switches[3]); */
  /* }}; */

  private final static SensorEvent[] switchSensor = new SensorEvent[] {
    // Switch 1
    new SensorEvent(-1, 16, 7, SensorEvent.INACTIVE),
    new SensorEvent(-1, 18, 7, SensorEvent.INACTIVE),    
    new SensorEvent(-1, 17, 8, SensorEvent.INACTIVE),    
    // Switch 2
    new SensorEvent(-1, 16, 9, SensorEvent.INACTIVE),
    new SensorEvent(-1, 15, 10, SensorEvent.INACTIVE),
    new SensorEvent(-1, 14, 9, SensorEvent.INACTIVE),
    // Switch 3
    new SensorEvent(-1, 5, 9, SensorEvent.INACTIVE),
    new SensorEvent(-1, 5, 10, SensorEvent.INACTIVE),
    new SensorEvent(-1, 3, 9, SensorEvent.INACTIVE),
    // Switch 4
    new SensorEvent(-1, 4, 11, SensorEvent.INACTIVE),
    new SensorEvent(-1, 3, 12, SensorEvent.INACTIVE),
    new SensorEvent(-1, 2, 11, SensorEvent.INACTIVE),

  };

  private final SensorEvent stationSensorUpper = new SensorEvent(-1, 15, 5, SensorEvent.INACTIVE);
  private final SensorEvent stationSensorLower = new SensorEvent(-1, 15, 13, SensorEvent.INACTIVE);


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

    System.out.println(this.trainID + " direction: " + this.direction);
    SensorEvent sensor = tsim.getSensor(this.trainID);
    
    if (sensor.getStatus() == SensorEvent.ACTIVE) {
      
      if(sensorEventEquals(sensor, stationSensorUpper) || sensorEventEquals(sensor, stationSensorLower)) {
        tsim.setSpeed(this.trainID, 0);
      }
      else
      {
        // SWITCH ONE
        if (sensorEventEquals(sensor, switchSensor[0]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), RIGHT); 
        }
        
        else if (sensorEventEquals(sensor, switchSensor[1]) && this.direction == LEFT) {
          tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), LEFT); 
        }
        
        else if (sensorEventEquals(sensor, switchSensor[2]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), RIGHT); 
        }
      
        // SWITCH TWO
        // can be both may need to changed later
        else if (sensorEventEquals(sensor, switchSensor[3]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), RIGHT); 
        }
        
        /* else if (sensorEventEquals(sensor, switchSensor[4]) && this.direction == LEFT) { */
        /*   tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), RIGHT);  */
        /* } */
        
        else if (sensorEventEquals(sensor, switchSensor[5]) && this.direction == LEFT) {
          tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), RIGHT); 
        }
       
        // SWITCH THREE 
        else if (sensorEventEquals(sensor, switchSensor[6]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), LEFT); 
        }
        
        else if (sensorEventEquals(sensor, switchSensor[7]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), LEFT); 
        }
        
        else if (sensorEventEquals(sensor, switchSensor[8]) && this.direction == LEFT) {
          tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), RIGHT); 
        }
      
        // SWITCH FOUR
        else if (sensorEventEquals(sensor, switchSensor[9]) && this.direction == LEFT) {
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), LEFT); 
        }
        
        else if (sensorEventEquals(sensor, switchSensor[10]) && this.direction == LEFT) {
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), LEFT); 
        }
        
        else if (sensorEventEquals(sensor, switchSensor[11]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), RIGHT); 
        }
      
      } 
    }
   }
}
