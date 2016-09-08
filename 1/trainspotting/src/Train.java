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
    // Switch 0 
    new SensorEvent(-1, 16, 7, SensorEvent.INACTIVE),
    new SensorEvent(-1, 18, 7, SensorEvent.INACTIVE),    
    new SensorEvent(-1, 17, 8, SensorEvent.INACTIVE),    
    // Switch 1
    new SensorEvent(-1, 16, 9, SensorEvent.INACTIVE),
    new SensorEvent(-1, 15, 10, SensorEvent.INACTIVE),
    new SensorEvent(-1, 14, 9, SensorEvent.INACTIVE),
    // Switch 2
    new SensorEvent(-1, 5, 9, SensorEvent.INACTIVE),
    new SensorEvent(-1, 5, 10, SensorEvent.INACTIVE),
    new SensorEvent(-1, 3, 9, SensorEvent.INACTIVE),
    // Switch 3
    new SensorEvent(-1, 4, 11, SensorEvent.INACTIVE),
    new SensorEvent(-1, 3, 12, SensorEvent.INACTIVE),
    new SensorEvent(-1, 2, 11, SensorEvent.INACTIVE),

  };

  private final static SensorEvent[] startEvent = new SensorEvent[] {
    new SensorEvent(-1, 7, 7, SensorEvent.INACTIVE),
    new SensorEvent(-1, 8, 6, SensorEvent.INACTIVE),
    new SensorEvent(-1, 9, 7, SensorEvent.INACTIVE),
    new SensorEvent(-1, 8, 8, SensorEvent.INACTIVE)
  };
    

  private final SensorEvent stationSensorUpper = new SensorEvent(-1, 15, 5, SensorEvent.INACTIVE);
  private final SensorEvent stationSensorLower = new SensorEvent(-1, 15, 13, SensorEvent.INACTIVE);

  private final static Semaphore stationUpperSema = new Semaphore(0);
  private final static Semaphore startionLowerSema = new Semaphore(0);

  private final static Semaphore[] semas = new Semaphore[] {
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1)
  }; 
  

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
  private boolean sensorEventEquals(SensorEvent s1, SensorEvent s2) {
	  if (s1.getXpos() == s2.getXpos() && s1.getYpos() == s2.getYpos()) return true;
	  else return false;
  }
 
  private boolean isStationEvent(SensorEvent s) {
    if (sensorEventEquals(s, stationSensorUpper) || sensorEventEquals(s, stationSensorLower)) return true;
    return false; 
  }
  
  private boolean isStartPointEvent(SensorEvent s) {
    for(SensorEvent e : startEvent) {
      if (sensorEventEquals(s, e)) return true;
    }
    return false; 
  } 

  private void requestSemaphore(int i) throws CommandException, InterruptedException {
    // may block
    tsim.setSpeed(this.trainID, 0);
    semas[i].acquire();
    tsim.setSpeed(this.trainID, this.trainSpeed);
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
      
      // Station EVENT
      if (isStationEvent(sensor)) {
        tsim.setSpeed(this.trainID, 0);
        sleep(1000 + (20 * this.trainSpeed));
        // change direction
        this.direction = (this.direction == LEFT) ? RIGHT : LEFT;
        this.trainSpeed = (this.trainSpeed > 0) ? -this.trainSpeed : Math.abs(this.trainSpeed);
        tsim.setSpeed(this.trainID, this.trainSpeed);
      }
      
      // Start Event 
      else if (isStartPointEvent(sensor)) {
        System.out.println("StartPointEvent");
        
        if (this.direction == RIGHT) {
        
          if (sensorEventEquals(sensor, startEvent[0])) {
            
            if (semas[1].tryAcquire()) {
              for (Semaphore s: semas) { 
                System.out.println(s.availablePermits());
              }
            }
            
            else {
              requestSemaphore(1);
            }
          
          }       
        
        }
        // IF EVENT IS SENSOR 1 && DIR== RIGHT THEN
        // CHECK IF SEMA 2,3,4  FREE (TRYACQUIRE)
        // TAKE THE SEMAPHORE AND RELEASE THE SEMAPHORE
        
        // IF EVENT IS SENSOR 2 && DIR==RIGHT THEN
        // CHECK IF SEMA 3,4 FREE
        // TAKE THE SEMAPHORE AND RELEASE THE SEMAPHORE

        // IF EVENT IS SENSOR 2 && DIR==LEFT THEN
        // CHECK IF SEMA 1
        // TAKE THE SEMAPHORE AND RELEASE THE SEMAPHORE

        // IF EVENT IS SENSOR 3 && DIR==LEFT THEN
        // CHECK IF SEMA 1
        // TAKE THE SEMAPHORE AND RELEASE THE SEMAPHORE

        // IF EVENT IS SENSOR 3 && DIR==LEFT THEN
        // CHECK IF SEMA 1
        // TAKE THE SEMAPHORE AND RELEASE THE SEMAPHORE


      }

      // SwitchSensor Event
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
          tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), LEFT); 
        }
      
        // SWITCH TWO
        // can be both may need to changed later
        else if (sensorEventEquals(sensor, switchSensor[3]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), RIGHT); 
        }
        
        // CHECK THIS ONE
        else if (sensorEventEquals(sensor, switchSensor[4]) && this.direction == LEFT) {
          tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), LEFT);
        }
        
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
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), RIGHT);
        }
        
        else if (sensorEventEquals(sensor, switchSensor[11]) && this.direction == RIGHT) {
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), RIGHT); 
        }
      
      }
    }

    if (sensor.getStatus() == SensorEvent.INACTIVE) {
      System.out.println("SENSOR INACTIVE");
    }  
 
  }
}
