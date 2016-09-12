import TSim.*; 
import java.util.concurrent.*;
import java.awt.Point;


public class Lab1 
{
	// Class variables
	public static final int LEFT = 0x01;
	public static final int RIGHT = 0x02;

	// Positions of the switches
	private final static Point[] switches = new Point[] {
		new Point(17, 7),
		new Point(15, 9),
		new Point(4, 9),
		new Point(3, 11)
	};

	// Critical Events
	private final static SensorEvent[] senseEvent = new SensorEvent[] {
		// Crossing Upper Station
		new SensorEvent(-1, 6, 6, SensorEvent.INACTIVE),
		new SensorEvent(-1, 8, 5, SensorEvent.INACTIVE),
		new SensorEvent(-1, 10, 7, SensorEvent.INACTIVE),
		new SensorEvent(-1, 10, 8, SensorEvent.INACTIVE),
		// MIddle Crossing
		new SensorEvent(-1, 14, 7, SensorEvent.INACTIVE),
		new SensorEvent(-1, 14, 8, SensorEvent.INACTIVE),
		new SensorEvent(-1, 19, 8, SensorEvent.INACTIVE),
		new SensorEvent(-1, 12, 9, SensorEvent.INACTIVE),
		new SensorEvent(-1, 12, 10, SensorEvent.INACTIVE),
		// Lower
		new SensorEvent(-1, 7, 9, SensorEvent.INACTIVE),
		new SensorEvent(-1, 7, 10, SensorEvent.INACTIVE),
		new SensorEvent(-1, 1, 10, SensorEvent.INACTIVE),
		new SensorEvent(-1, 5, 11, SensorEvent.INACTIVE),
		new SensorEvent(-1, 5, 13, SensorEvent.INACTIVE)
	};

	private final static SensorEvent[] upperHomeEvent = new SensorEvent[] {
		new SensorEvent(-1, 11, 3, SensorEvent.INACTIVE),
		new SensorEvent(-1, 11, 5, SensorEvent.INACTIVE)
	};

	private final static SensorEvent[] lowerHomeEvent = new SensorEvent[] {
		new SensorEvent(-1, 11, 11, SensorEvent.INACTIVE),
		new SensorEvent(-1, 11, 13, SensorEvent.INACTIVE)
	};

	private final static Semaphore homeUpper1 = new Semaphore(0);
	private final static Semaphore homeUpper2 = new Semaphore(1);
  private final static Semaphore mergeLow = new Semaphore(1);
	private final static Semaphore homeLower = new Semaphore(0);
  private final static Semaphore crossing = new Semaphore(1);
	private final static Semaphore mid = new Semaphore(1);
	private final static Semaphore dualLanes = new Semaphore(1);


	public Lab1(Integer speed1, Integer speed2) 
	{
		// create two objects that execute in individual thread
		Train t1 = new Train(1, speed1, 2);
		Train t2 = new Train(2, speed2, 1);

		// start threads 
		t1.start();
		t2.start();

		// wait for threads to finish but they will never finish
		try {
			t1.join();
			t2.join();
		}
		catch (InterruptedException e) 
		{
			e.getMessage();
		} 
	}

	public class Train extends Thread
	{
		// Instance Variables
		private TSimInterface tsim;
		private int trainID;
		private int trainSpeed;
		private int direction;

		
		/**
		 * Constructor for Train class each object as an individual thread
		 *
		 * @throws IllegalArgumentException
		 *             when the tdir is not 1 or 2
		 **/
		public Train(int tID, int tSpeed, int tDir) 
		{
			if ( tDir != 1 && tDir != 2) 
			{
				throw new IllegalArgumentException("Direction must be either 1 or 2");
			}
			this.trainID = tID;
			this.trainSpeed = tSpeed;
			this.direction = tDir;
			tsim = TSimInterface.getInstance();
		}

		/**
		 * 		Event Loop for the Train
		 *
		 **/
		public void run()
		{
			try 
			{
				tsim.setSpeed(this.trainID, this.trainSpeed);
			}
			catch (CommandException e) 
			{
				e.printStackTrace();
				System.exit(1);
			}
			// event loop
			while (true) 
			{
				try 
				{
					update();
				}
				catch (CommandException | InterruptedException e) 
				{
					e.getMessage();
				}
			}
		}

		/**
		 * Determines if the SensorEvent is an UpperStationEvent
		 *
		 * @param SensorEvent
		 * 
		 * @return true if the SensorEvent is an UpperStationEvent
		 * 									false if the SensorEvent is not an UpperStationEvent
		 **/
		private boolean isUpperStationEvent(SensorEvent s) 
		{
			for(SensorEvent e : upperHomeEvent) {
				if (sensorEventEquals(s, e)) return true;
			}
			return false;
		}

		/**
		 * Determines if the SensorEvent is a LowerStationEvent
		 *
		 * @param SensorEvent
		 * 
		 * @return true if the SensorEvent is a LowerStationEvent
		 * 									false if the SensorEvent is not an LowerStationEvent
		 **/
		private boolean isLowerStationEvent(SensorEvent s) 
		{
			for(SensorEvent e : lowerHomeEvent) {
				if (sensorEventEquals(s, e)) return true;
			}
			return false;
		}


		/**
		 * Compares two SensorEvent based on Xpos and Ypos
		 *
		 * @param SensorEvent s1 s2
		 * 
		 * @return true if the SensorEvents are equal
		 * 									false if the SensorEvents are not equal
		 **/
		private boolean sensorEventEquals(SensorEvent s1, SensorEvent s2) 
    {
			if (s1.getXpos() == s2.getXpos() && s1.getYpos() == s2.getYpos()) return true;
			else return false;
		}

		/**
		 * Determines if a SensorEvent is critical
		 *
		 * @param SensorEvent s
		 * 
		 * @return  true if the SensorEvent is critical
		 * 									false if the SensorEvents is not critical
		 **/
		private boolean isCriticalEvent(SensorEvent s) 
		{
			for(SensorEvent e : senseEvent) {
				if (sensorEventEquals(s, e)) return true;
			}
			return false;
		}
		
		/**
		 * Suspends the Train for a short while and changes the direction
		 *
		 **/
		private void changeDirection() throws CommandException, InterruptedException
		{
			tsim.setSpeed(this.trainID, 0);
			sleep(2000 + (20 * this.trainSpeed));
			// change direction
			this.direction = (this.direction == LEFT) ? RIGHT : LEFT;
			this.trainSpeed = (this.trainSpeed > 0) ? -this.trainSpeed : Math.abs(this.trainSpeed);
			tsim.setSpeed(this.trainID, this.trainSpeed);
		}

		/**
		 * Request for a Sempahore s and wait until it is available
		 *
		 * @throws  CommandException, InterruptedException
		 **/
		private void requestSemaphore(Semaphore s) throws CommandException, InterruptedException {
			// may block
			tsim.setSpeed(this.trainID, 0);
			s.acquire();
			tsim.setSpeed(this.trainID, this.trainSpeed);
		}

		/**
		 * Handles events for the Train
		 *
		 * @throws  CommandException, InterruptedException, IllegalArgumentExpression
		 **/
		private void update() throws CommandException, InterruptedException {
			if (this.direction != 1 && this.direction != 2)
			{
				throw new IllegalArgumentException("Direction must be either 1 or 2");
			}

			SensorEvent sensor = tsim.getSensor(this.trainID);

			if (sensor.getStatus() == SensorEvent.ACTIVE) 
      {
				// UpperStation EVENT
				if (isUpperStationEvent(sensor))
				{
					if (this.direction == LEFT) changeDirection();
				}
        // LowerStation Event
				else if (isLowerStationEvent(sensor))
				{
					if (this.direction == RIGHT) changeDirection();
				}
				// Sensor Event
				else if (isCriticalEvent(sensor))
				{
					criticalEventHandler(sensor);
				}
			}
		}
		
		/**
		 * 	Handles critical events for the Train,
		 * A lot of code do not comply with the DRY principle......... 
		 *
		 * @throws  CommandException, InterruptedExceptioN
		 **/
		private void criticalEventHandler(SensorEvent sensor) throws CommandException, InterruptedException
		{
			// HANDLE ALL EVENTS FROM GOING DOWN
			if (this.direction == RIGHT) 
			{
				if (sensorEventEquals(sensor, senseEvent[0]) || sensorEventEquals(sensor, senseEvent[1]))
				{
					requestSemaphore(crossing);
				}     
				else if (sensorEventEquals(sensor, senseEvent[2]) ||  sensorEventEquals(sensor, senseEvent[3])   )
				{
					crossing.release();
				}
				else if (sensorEventEquals(sensor, senseEvent[4]))
				{
					requestSemaphore(mid);
					homeUpper1.release();
					tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), RIGHT);
					setDualLaneDirection(RIGHT);
				}    
				else if (sensorEventEquals(sensor, senseEvent[5]))
				{
					requestSemaphore(mid);
					homeUpper2.release();
					tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), LEFT);
					setDualLaneDirection(RIGHT);
				}
				else if (sensorEventEquals(sensor, senseEvent[7]) || (sensorEventEquals(sensor, senseEvent[8])))
				{
					mid.release();
				}
				
        else if (sensorEventEquals(sensor, senseEvent[9]))
				{
					requestSemaphore(mergeLow);
					tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), LEFT);
					dualLanes.release();
				}
				
        else if (sensorEventEquals(sensor, senseEvent[10]))
				{
					requestSemaphore(mergeLow);
					tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), RIGHT);
				}     
				
        else if (sensorEventEquals(sensor, senseEvent[11]))
				{
					if (homeLower.tryAcquire()) 
          {
					  tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), LEFT);
          }
          else 
          {
					  tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), RIGHT);
          }
				}     

				else if (sensorEventEquals(sensor, senseEvent[12]) || sensorEventEquals(sensor, senseEvent[13]))
				{
          mergeLow.release();
				}     
			}

			// HANDLE EVENT GOING UP
			else if (this.direction == LEFT) 
			{
				if (sensorEventEquals(sensor, senseEvent[0]) || sensorEventEquals(sensor, senseEvent[1]))
				{
					crossing.release();
				}
				else if (sensorEventEquals(sensor, senseEvent[2]) || sensorEventEquals(sensor, senseEvent[3]))
				{
					requestSemaphore(crossing);
				}
				else if (sensorEventEquals(sensor, senseEvent[4]) || sensorEventEquals(sensor, senseEvent[5]))
				{
					mid.release();
				}

				else if (sensorEventEquals(sensor, senseEvent[6]))
				{
					if (homeUpper2.tryAcquire()) 
					{
						tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), LEFT);
					}
					else 
					{
						homeUpper1.tryAcquire(); 
						tsim.setSwitch((int)switches[0].getX(), (int)switches[0].getY(), RIGHT);
					}
				}
				else if (sensorEventEquals(sensor, senseEvent[7]))
				{
					requestSemaphore(mid);
					tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), RIGHT);
					dualLanes.release();
				}
				else if (sensorEventEquals(sensor, senseEvent[8]))
				{
					requestSemaphore(mid);
					tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), LEFT);
				}
				else if (sensorEventEquals(sensor, senseEvent[9]) || sensorEventEquals(sensor, senseEvent[10]))
				{
					mergeLow.release();
				}
				
        else if (sensorEventEquals(sensor, senseEvent[11]))
				{
					setDualLaneDirection(LEFT);
				}
				
        else if (sensorEventEquals(sensor, senseEvent[12]))
				{
          requestSemaphore(mergeLow);
          homeLower.release();
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), LEFT);
        }
        
        else if (sensorEventEquals(sensor, senseEvent[13]))
				{
					requestSemaphore(mergeLow);
          tsim.setSwitch((int)switches[3].getX(), (int)switches[3].getY(), RIGHT);
        }
			}
		}  
		
		/**
		 * 	Set direction on the dual lane to introduce some asymmetry to avoid deadlocks etc.
		 * E.g. if Train 1 aquires the dualLane semaphore it will run on the upper lane and Train 2 on the lower lane
		 *
		 * @throws  CommandException, InterruptedException, IllegalArgumentException
		 **/
		private void setDualLaneDirection(int direction) throws CommandException, InterruptedException
		{
			if ( direction != 1 && direction != 2)
			{
			  throw new IllegalArgumentException("Direction must be either 1 or 2");
      }
			if (dualLanes.tryAcquire()) 
			{
				if (direction == RIGHT) tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), RIGHT);
				else tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), LEFT);
			}
			else
			{
				if (direction == RIGHT) tsim.setSwitch((int)switches[1].getX(), (int)switches[1].getY(), LEFT);
				else tsim.setSwitch((int)switches[2].getX(), (int)switches[2].getY(), RIGHT);
			}
		}
	}
}
