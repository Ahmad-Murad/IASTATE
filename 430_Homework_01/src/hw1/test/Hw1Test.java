package hw1.test;

import static org.junit.Assert.*;
import hw1.ImmutableTrajectory;
import hw1.Trajectory;

import java.awt.Point;

import org.junit.Test;

public class Hw1Test {

	@Test
	public void testImmutiable1(){
		Point[] arr = { new Point(0,0), new Point(0,0), new Point(0,0)};
		
		ImmutableTrajectory traj = new ImmutableTrajectory(arr);
		
		Point[] newArr = traj.getValues();
		newArr[0] = new Point(1,1);
		
		assertEquals(new Point(0,0), arr[0]);
	}
	
	@Test
	public void testTrajector1() throws InterruptedException{
		// need to insert delays into Trajectory code to observe results
		Point[] arr = { new Point(0,0), new Point(0,0), new Point(0,0)};
		Trajectory traj = new Trajectory(arr);
		
		Thread t1 = new Thread(new Runnable(){
			@Override
			public void run() {
				Point[] result = traj.getValues();
				for(Point p : result)
					System.out.println(p);
			}
		});
		Thread t2 = new Thread(new Runnable(){
			@Override
			public void run() {
				traj.update(1, new Point(1,1));
				System.out.println("Set point 1 to [x=1,y=1]");
			}
		});
		
		t1.start();
		t2.start();
		
		t1.join();
		t2.join();
	}
}
