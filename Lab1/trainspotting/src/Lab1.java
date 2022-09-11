import TSim.*;

import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Lab1 {
  private HashMap<String, Semaphore> semaphores = new HashMap<>();

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    putSemaphores();

    try {

      Train train1 = new Train(1, speed1, semaphores, "stat_A1");
      Train train2 = new Train(2, speed2, semaphores, "stat_B1");

      //tsi.setSwitch(17,7,0);   //Points up with 0
      //tsi.setSwitch(15,9,0);   //Points up with 0
      //tsi.setSwitch(4, 9, 1);  //Points up with 1
      //tsi.setSwitch(3, 11, 1); //Points up with 1

      train1.start();
      train2.start();

      train1.join();
      train2.join();
    }
    catch (Exception e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }
  public void putSemaphores() {
    semaphores.put("stat_A1", new Semaphore(1));
    semaphores.put("stat_A2", new Semaphore(1));
    semaphores.put("stat_B1", new Semaphore(1));
    semaphores.put("stat_B2", new Semaphore(1));

    semaphores.put("crossing", new Semaphore(1));
    semaphores.put("track_A", new Semaphore(1));
    semaphores.put("track_B", new Semaphore(1));

    semaphores.put("track_B1", new Semaphore(1));
    semaphores.put("track_B2", new Semaphore(1));
  }
}
