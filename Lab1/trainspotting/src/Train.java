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
        }
        catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
