import TSim.*;

public class Lab1 {

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    try {

      Train train1 = new Train(1, speed1);
      Train train2 = new Train(2, speed2);

      train1.start();
      train2.start();

      //tsi.setSpeed(2,speed2);
    }
    catch (Exception e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }
}
