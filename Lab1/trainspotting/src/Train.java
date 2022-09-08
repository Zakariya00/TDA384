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
    private Direction direction;
    private boolean upperTrack;
    private boolean waiting = false;

    public Train (int id, int speed, HashMap<String, Semaphore> semaphores, String currentSem)  {
        this.id = id;
        this.speed = speed;
        this.semaphores = semaphores;
        this.currentSem = currentSem;
        this.lastSem = currentSem;
        this.upperTrack = true;
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

        if (event.getStatus() == SensorEvent.ACTIVE) {

            // From A to B
            if (direction == Direction.A_B) {
                if ((x == 6 && y == 7) || (x == 8 && y == 5)) {  // Acquire crossing
                    quickStop();
                    while (!semAcq("crossing")) sleep (250);
                    quickStart();
                }

                if ((x == 12 && y == 7) || (x == 12 && y == 8)) { // Release crossing and Acquire track_A
                    semRel("crossing");
                    quickStop();
                    while (!semAcq("track_A")) sleep(250);

                    if (upperTrack) tsi.setSwitch(17, 7, 0);
                    else if (!upperTrack) tsi.setSwitch(17, 7, 1);
                    quickStart();
                }

                if (x == 19 && y == 8) { // Release Higher Stations and Acquire next Track
                    if (upperTrack) semRel("stat_A1");
                    else if (!upperTrack) semRel("stat_A2");
                    if (!semAcq("stat_B1")) semAcq("stat_B2");

                    if (upperTrack) tsi.setSwitch(15, 9, 0);
                    else if (!upperTrack) tsi.setSwitch(15, 9, 1);
                }

                if ((x == 9 && y == 9) || (x == 9 && y == 10)) { // Acquire track_B
                    semRel("track_A");
                    quickStop();
                    while (!semAcq("track_B")) sleep(250);

                    if (upperTrack) tsi.setSwitch(4, 9, 1);
                    else if (!upperTrack) tsi.setSwitch(4, 9, 0);
                    quickStart();
                }

                if ((x == 1 && y == 10)) { // Acquire Lower Stations
                    if (upperTrack) semRel("track_B1");
                    else if (!upperTrack) semRel("track_B2");
                    if(!semAcq("stat_B1")) semAcq("stat_B2");

                    if (upperTrack) tsi.setSwitch(3, 11, 1);
                    else if (!upperTrack) tsi.setSwitch(3, 11, 0);
                }

                if ((x== 4 && y == 13) || (x == 5 && y == 11)) semRel("track_B");

                if ((x == 14 && y == 11) || (x == 14 && y == 13)) // Stopping at Lower Stations
                    stopProtocol();

            }

            // From B to A
            if (direction == Direction.B_A) {
                if ((x == 5 && y == 11) || (x == 4 && y == 13)) {   //Acquire track_B
                    quickStop();
                    while (!semAcq("track_B")) sleep(250);

                    if (upperTrack) tsi.setSwitch(3, 11, 1);
                    else if (!upperTrack) tsi.setSwitch(3, 11, 0);
                    quickStart();
                }

                if (x == 1 && y == 10) { // Release Lower Stations and Acquire next Track
                    if (upperTrack) semRel("stat_B1");
                    else if (!upperTrack) semRel("stat_B2");
                    if (!semAcq("track_B1")) semAcq("track_B2");

                    if (upperTrack) tsi.setSwitch(4, 9, 1);
                    else if (!upperTrack) tsi.setSwitch(4, 9, 0);
                }

                if ((x == 9 && y == 9) || (x == 9 && y == 10)) { // Acquire track_A
                    semRel("track_B");
                    quickStop();
                    while (!semAcq("track_A")) sleep(250);

                    if (upperTrack) tsi.setSwitch(15, 9, 0);
                    else if (!upperTrack) tsi.setSwitch(15, 9, 1);
                    quickStart();
                }

                if ((x == 19 && y == 8)) {  // Acquire Higher Stations
                    if (upperTrack) semRel("track_B1");
                    else if (!upperTrack) semRel("track_B2");
                    if (!semAcq("stat_A1")) semAcq("stat_A2");

                    if (upperTrack) tsi.setSwitch(17, 7, 0);
                    else if (!upperTrack) tsi.setSwitch(17, 7, 1);
                }

                if ((x == 12 && y == 7) || (x == 12 && y == 8)) {  // Acquire crossing
                    semRel("track_A");
                    quickStop();
                    while (!semAcq("crossing")) sleep (250);
                    quickStart();
                }

                if ((x == 6 && y == 7) || (x == 8 && y == 5))  // Release crossing
                    semRel("crossing");

                if ((x == 14 && y == 3) || (x == 14 && y == 5)) // Stopping at Higher Stations
                    stopProtocol();

            }
        }
    }


    public void startProtocol () throws CommandException {
        //semAcq(this.currentSem);

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

        //semRel(this.currentSem);
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
                if (semName.equals("stat_A1") || semName.equals("track_B1") || semName.equals("stat_B1"))
                    upperTrack = true;
                else if (semName.equals("stat_A2") || semName.equals("track_B2") || semName.equals("stat_B2"))
                    upperTrack = false;
                System.out.println(upperTrack);
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


   /* public void processTrain (SensorEvent event) throws CommandException, InterruptedException {
        int x = event.getXpos();
        int y = event.getYpos();

        if (event.getStatus() == SensorEvent.ACTIVE) {

            //Stopping at Stations
            if ((((x == 14 && y == 3) || (x == 14 && y == 5)) && direction == Direction.B_A) ||
                       (((x == 14 && y == 11) || (x == 14 && y == 13)) && direction == Direction.A_B)) {
                stopProtocol();
            }
            //Setting Switches A_B
            if (direction == Direction.A_B) {
                if ((x == 12 && y == 7) || (x == 12 && y == 8) || (x == 19 && y == 8) || (x == 9 && y == 9) || (x == 9 && y == 10) || (x == 1 && y == 10)) {
                    changeSwitch(x, y, direction);
                }}

            //Setting Switches B_A
            if (direction == Direction.B_A) {
                if ((x == 14 && y == 13) || (5 == 12 && y == 11) || (x == 1 && y == 10) || (x == 9 && y == 9) || (x == 9 && y == 10) || (x == 19 && y == 8)) {
                    changeSwitch(x, y, direction);
                }}

            //Crossing Sections

        }
    }

    private void changeSwitch (int x, int y, Direction dir) {

        }
    }*/

   /* private boolean nextTrack (String trackSem) throws CommandException, InterruptedException {
        quickStop();
        if (semAcq(trackSem)) {
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
    }*/


