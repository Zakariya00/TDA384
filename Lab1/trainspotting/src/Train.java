import TSim.SensorEvent;
import TSim.TSimInterface;

public class Train extends Thread {
    private TSimInterface tsi = TSimInterface.getInstance();

    private int id;
    private int speed;

    public Train (int id, int speed)  {
        this.id = id;
        this.speed = speed;
    }

    @Override
    public void run () {
        try {
            tsi.setSpeed(this.id, this.speed);
            //while (true)
            //  processTrain (tsi.getSensor(this.id));
        }
        catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }


    public static void processTrain (SensorEvent event) {
        throw new UnsupportedOperationException ("Not implemented, yet");
    }

}
