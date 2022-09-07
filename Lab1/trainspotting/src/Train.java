import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Train extends Thread {
    private TSimInterface tsi = TSimInterface.getInstance();
    private HashMap<String, Semaphore> semaphores;
    enum Direction { A_B, B_A, Unknown }

    private int id;
    private int speed;
    private String currentSem;
    private String lastSem = "NULL";
    private Direction direction;
    private boolean running = false;

    public Train (int id, int speed, HashMap<String, Semaphore> semaphores, String currentSem)  {
        this.id = id;
        this.speed = speed;
        this.semaphores = semaphores;
        this.currentSem = currentSem;
    }

    @Override
    public void run () {
        try {
            initalDir();
            startProtocol ();

            while (true)
                processTrain (tsi.getSensor(this.id));
        }
        catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }




    public void processTrain (SensorEvent event) throws CommandException, InterruptedException {
            int x = event.getXpos();
            int y = event.getYpos();

        if (((x == 14 && y == 3) || (x == 14 && y == 5)) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.B_A) {
            stopProtocol();
            }
            else
                semRel(this.currentSem);
        }

        if (((x == 14 && y == 11) || (x == 14 && y == 13)) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
            stopProtocol();
            }
            else
                semRel(this.currentSem);
        }

        if ((x == 2 && y == 9) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
                if (semAcq("stat_B1"))
                    tsi.setSwitch(3, 11, 1);
                else {
                    semAcq("stat_B2");
                    tsi.setSwitch(3, 11, 0);
                }
            }
        }

        if ((x == 17 && y == 9) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.B_A) {
                if (semAcq("stat_A1"))
                    tsi.setSwitch(17, 7, 0);
                else {
                    semAcq("stat_A2");
                    tsi.setSwitch(17, 7, 1);
                }
            }
        }
    }


    public void startProtocol () throws CommandException {
        semAcq(this.currentSem);

        tsi.setSpeed(this.id, this.speed);
        System.out.println("" + this.id + " Started" );

    }
    public void stopProtocol () throws CommandException, InterruptedException {

        tsi.setSpeed(this.id, 0);
        this.running = false;

        System.out.println("" + this.id + " Stopped" );

        int delay = 1000 + (20 * Math.abs(this.speed));
        this.speed = this.speed * -1;

        sleep(delay);
        //semRel(this.currentSem);
        switchDir();
        startProtocol();

    }
    public void initalDir () {
        if ( (this.currentSem.equals("stat_A1")) || (this.currentSem.equals("stat_A2")) ) {
            this.direction = Direction.A_B;
        }
        else {
            this.direction = Direction.B_A;
        }
    }
    public void switchDir () {
        if (this.direction == Direction.A_B)
            this.direction = Direction.B_A;
        else
            this.direction = Direction.A_B;
    }

    public void quickStart () throws CommandException { tsi.setSpeed(this.id, this.speed); this.running = true;}
    public void quickStop () throws CommandException { tsi.setSpeed(this.id, 0); this.running = false;}

    private boolean semAcq (String semName) {
        if (semaphores.get(semName).tryAcquire()) {
            this.lastSem = currentSem;
            this.currentSem = semName;
            System.out.println("" + semName + " acquired");
            return true;
        }
        else
            return false;
    }
    private boolean semRel (String semName) {
        Semaphore sem = semaphores.get(semName);
        if (sem.availablePermits() > 0)
            return false;
        else
            sem.release();
        System.out.println("" + semName + " released");
        return true;
    }

}
