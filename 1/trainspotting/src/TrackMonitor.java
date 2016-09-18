import java.util.concurrent.locks.*;

import TSim.CommandException;

public class TrackMonitor 
{
	private final Lock lock = new ReentrantLock();	
  // used mostly for debugging
  private String name;
	private final Condition mutex = lock.newCondition();
	private int permits; 
	
	public TrackMonitor()
	{
		this.permits = 1;
	}
	
	public TrackMonitor(String name, int permits)
	{
		if (permits < 0 || permits > 1) throw new IllegalArgumentException("Permit must be either 0 or 1");
		this.permits = permits;
	}
	
	
	public void enter() throws InterruptedException
	{
		if (this.permits < 0 || this.permits > 1) throw new IllegalArgumentException("Permit must be either 0 or 1");
    try 
		{
			lock.lock();
			/* Wait until permit is at least 1 i.e. allowed to enter */
			while (this.permits < 1)
			{
				mutex.await();
			}
			/* Got the conditional variable */
			this.permits--;
		}
		catch (InterruptedException e) 
		{
			e.getMessage();
		}
		
		finally 
		{
			lock.unlock();
		}
	}
	
	public void leave()
	{
			if (permits != 0) throw new IllegalArgumentException("Permit must be ZERO to leave");
			lock.lock();
			this.permits++;
			mutex.signal();
      lock.unlock();
	}
	
	public boolean tryAcquire()
	{
		if (permits < 0 || permits > 1) throw new IllegalArgumentException("Permit must be either 0 or 1");
    lock.lock();
		boolean ret = false;
		if (this.permits == 1)
		{
			this.permits--;
			ret = true;
		}
		lock.unlock();
		return ret;
	}

 private void Debug(String func)
 {
  System.out.println(func + " " + name + " Permit: " + this.permits);
 }
}
