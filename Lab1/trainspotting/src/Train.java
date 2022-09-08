import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Train extends Thread {
    private TSimInterface tsi = TSimInterface.getInstance();
    private HashMap<String, Semaphore> semaphores;
    enum Direction { A_B, B_A}

    private int id;
    private int speed;
    private String currentSem;
    private String lastSem;
    private String futureSem;
    private Direction direction;
    private boolean waiting = false;

    public Train (int id, int speed, HashMap<String, Semaphore> semaphores, String currentSem)  {
        this.id = id;
        this.speed = speed;
        this.semaphores = semaphores;
        this.currentSem = currentSem;
        this.lastSem = currentSem;
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

         // For Entering Stations
        if (((x == 14 && y == 3) && event.getStatus() == SensorEvent.ACTIVE) ||
                      ((x == 14 && y == 5) && event.getStatus() == SensorEvent.ACTIVE)) {
            if (this.direction == Direction.B_A)
                stopProtocol();
        }
        if (((x == 14 && y == 11) && event.getStatus() == SensorEvent.ACTIVE) ||
                      ((x == 14 && y == 13) && event.getStatus() == SensorEvent.ACTIVE)) {
            if (this.direction == Direction.A_B)
                stopProtocol();
        }

        // For crossing
        if ((x == 6 && y == 7) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
                quickStop();
                    while (!semAcq("crossing"))
                        sleep(250);
                    quickStart();
                }
            else
                semRel("crossing");
        }
        if ((x == 8 && y == 5) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
                quickStop();
                while (!semAcq("crossing"))
                    sleep(250);
                quickStart();
            }
            else
                semRel("crossing");
        }

         // For Leaving Stations
        if ((x == 12 && y == 7) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
                semRel("crossing");
                nextTrack("track_A");
            }
            else {
                quickStop();
                while (!semAcq("crossing"))
                    sleep(250);
                quickStart();
            }
        }
        if ((x == 12 && y == 8) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
                semRel("crossing");
                nextTrack("track_A");
            }
            else {
                quickStop();
                while (semAcq("crossing"))
                    sleep(250);
                quickStart();
            }
        }

        if ((x == 5 && y == 11) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.B_A)
                nextTrack("track_B");
        }
        if ((x == 4 && y == 13) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.B_A)
                nextTrack("track_B");
        }

        if ((x == 9 && y == 9) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {nextTrack("track_B"); }
            else {nextTrack("track_A");}
        }
        if ((x == 9 && y == 10) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {nextTrack("track_B");}
            else {nextTrack("track_A");}
        }

        if ((x == 19 && y == 8) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B)
                if (!nextTrack("track_B1")) {nextTrack("track_B2");}
            else {
                if (!nextTrack("stat_A1")) nextTrack("stat_A2");}
        }
        if ((x == 1 && y == 10) && event.getStatus() == SensorEvent.ACTIVE) {
            if (this.direction == Direction.A_B) {
                if (!nextTrack("stat_B1")) nextTrack("stat_B2");}
            else {
                if (!nextTrack("track_B1")) nextTrack("track_B2");}
        }

    }

    private void atStation () throws CommandException, InterruptedException {
        stopProtocol();
    }
    private boolean nextTrack (String trackSem) throws CommandException, InterruptedException {
        quickStop();
        if (trackSem.equals("track_A") || trackSem.equals("track_B")) {
            while (!semAcq(trackSem))
                sleep(250);
            semRel(this.lastSem);
            switch4Track();
            quickStart();
            System.out.println("Track Acquired by Train " + this.id);
            return true;
        }
        else if (semAcq(trackSem)) {
            semRel(this.lastSem);
            switch4Track();
            quickStart();
            System.out.println("Track Acquired by Train " + this.id);
            return true;
        }
        System.out.println("Wait for Track Train " + this.id);
        return false;
    }
    private void switch4Track () throws CommandException {
        // Direction B to A
        if (lastSem.equals("stat_B1") && currentSem.equals("track_B"))
            tsi.setSwitch(3, 11, 1);
        if (lastSem.equals("stat_B2") && currentSem.equals("track_B"))
            tsi.setSwitch(3, 11, 0);

        if (lastSem.equals("track_B") && currentSem.equals("track_B1"))
            tsi.setSwitch(4, 9, 1);
        if (lastSem.equals("track_B") && currentSem.equals("track_B2"))
            tsi.setSwitch(4, 9, 0);

        if (lastSem.equals("track_B1") && currentSem.equals("track_A"))
            tsi.setSwitch(15, 9, 0);
        if (lastSem.equals("track_B2") && currentSem.equals("track_A"))
            tsi.setSwitch(15, 9, 1);

        if (lastSem.equals("track_A") && currentSem.equals("stat_A1"))
            tsi.setSwitch(17, 7, 0);
        if (lastSem.equals("track_A") && currentSem.equals("stat_A2"))
            tsi.setSwitch(17, 7, 1);

        // Direction A to B
        if (lastSem.equals("stat_A1") && currentSem.equals("track_A")) // ---
            tsi.setSwitch(17, 7, 0);
        if (lastSem.equals("stat_A2") && currentSem.equals("track_A"))
            tsi.setSwitch(17, 7, 1);

        if (lastSem.equals("track_A") && currentSem.equals("track_B1"))
            tsi.setSwitch(15, 9, 0);
        if (lastSem.equals("track_A") && currentSem.equals("track_B2"))
            tsi.setSwitch(15, 9, 1);

        if (lastSem.equals("track_B1") && currentSem.equals("track_B"))
            tsi.setSwitch(4, 9, 1);
        if (lastSem.equals("track_B2") && currentSem.equals("track_B"))
            tsi.setSwitch(4, 9, 0);

        if (lastSem.equals("track_B") && currentSem.equals("stat_B1"))
            tsi.setSwitch(3, 11, 1);
        if (lastSem.equals("track_B") && currentSem.equals("stat_B2"))
            tsi.setSwitch(3, 11, 0);
    }



    public void startProtocol () throws CommandException {
        semAcq(this.currentSem);

        tsi.setSpeed(this.id, this.speed);
        System.out.println("" + this.id + " Started" );

    }
    public void stopProtocol () throws CommandException, InterruptedException {
        tsi.setSpeed(this.id, 0);
        System.out.println("" + this.id + " Stopped" );

        int delay = 1000 + (20 * Math.abs(this.speed));
        this.speed = this.speed * -1;
        sleep(delay);

        switchDir();
        semRel(this.currentSem);
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
    public void quickStart () throws CommandException { tsi.setSpeed(this.id, this.speed); waiting = false;}
    public void quickStop () throws CommandException { tsi.setSpeed(this.id, 0); waiting = true;}


    private boolean semAcq (String semName) {
        if (semaphores.get(semName).tryAcquire()) {
            if (!semName.equals("crossing")) {
            this.lastSem = currentSem;
            this.currentSem = semName;
            }
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
