import java.util.concurrent.locks.*;

import TSim.CommandException;

public class TrackMonitor 
{
	private final Lock lock = new ReentrantLock();	
  // used mostly for debugging
  private String name;
	private final Condition mutex = lock.newCondition();
	private int token; 
	
	public TrackMonitor()
	{
		this.token = 1;
	}
	
	public TrackMonitor(String name, int token)
	{
		if (this.token < 0 || this.token > 1) throw new IllegalArgumentException("Permit must be either 0 or 1");
		this.token = token;
    this.name = name;
	}
	
	
	public void enter() throws InterruptedException
	{
		if (this.token < 0 || this.token > 1) throw new IllegalArgumentException("Permit must be either 0 or 1");
    try 
		{
			lock.lock();
			Debug("enter()");
      /* Wait until permit is at least 1 i.e. allowed to enter */
			while (this.token != 1) 
			{
				mutex.await();
			}
			/* Got the conditional variable */
			this.token--;
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
			if (this.token != 0) throw new IllegalArgumentException("Permit must be ZERO to leave");
			lock.lock();
			this.token++;
			mutex.signal();
      lock.unlock();
	}
	
	public boolean tryAcquire()
	{
		if (this.token < 0 || this.token > 1) throw new IllegalArgumentException("Permit must be either 0 or 1");
    lock.lock();
    boolean ret = false;
		if (this.token == 1)
		{
			this.token--;
			ret = true;
		}
		lock.unlock();
		return ret;
	}

 private void Debug(String func)
 {
  System.out.println(func + " " + this.name + " Permit: " + this.token);
 }
}
