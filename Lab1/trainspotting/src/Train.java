import TSim.CommandException;
import TSim.SensorEvent;
import TSim.TSimInterface;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Train extends Thread {
    private TSimInterface tsi = TSimInterface.getInstance();
    private HashMap<String, Semaphore> semaphores;                   // The track Semaphores
    private ArrayList<Semaphore> mySemaphores = new ArrayList<>();  // The semaphores the train is holding
    enum Direction { A_B, B_A}                                     // Stations [A to B] or [B to A]

    private int id;
    private int speed;
    private String startStation;
    private Direction direction;
    private int lastX;                                         // Updated with X-Pos of last affected sensor
    private int lastY;                                        // Updated with X-Pos of last affected sensor


    public Train (int id, int speed, HashMap<String, Semaphore> semaphores, String startStation)  {
        this.id = id;
        this.speed = speed;
        this.startStation = startStation;
        this.semaphores = semaphores;
    }

    @Override
    public void run () {
        try {
            semAcq(startStation); // Acquire the start Station
            initalDir();         // Set the initial Direction
            startProtocol ();   // Start the train

            while (true)
              processTrain (tsi.getSensor(this.id)); // Process sensor events for the specific train
        }
        catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }



    // Main method for handling the train, action taken based on what sensor train is affecting and current state
    private void processTrain (SensorEvent event) throws CommandException, InterruptedException {
        int x = event.getXpos();
        int y = event.getYpos();

        if (event.getStatus() == SensorEvent.ACTIVE) {
            lastX = x;
            lastY = y;

            // From A to B
            if (direction == Direction.A_B) {
                if ((x == 6 && y == 7) || (x == 8 && y == 5)) {  // Acquire crossing
                    //singleTrack("crossing");
                    theCrossing();
                }

                if ((x == 12 && y == 7) || (x == 12 && y == 8)) { // Release crossing & Acquire track_A
                    semRel("crossing");
                    singleTrack("track_A");
                }

                if (x == 19 && y == 8) { // Release Higher Stations & Acquire next Track [Double Track]
                    semRel("stat_A1"); semRel("stat_A2");
                    multiTrack("track_B1", "track_B2");
                }

                if ((x == 9 && y == 9) || (x == 9 && y == 10)) { // Release track_A track & Acquire track_B
                    semRel("track_A");
                    singleTrack("track_B");
                }

                if ((x == 1 && y == 10)) { //  Release double track & Acquire Lower Stations
                    semRel("track_B1"); semRel("track_B2");
                    multiTrack("stat_B1", "stat_B2");
                }

                if ((x== 4 && y == 13) || (x == 5 && y == 11)) { // Relaease track_B
                    semRel("track_B");
                }

                if ((x == 14 && y == 11) || (x == 14 && y == 13)) {  // Stop at Lower Stations & stop protocol
                    stopProtocol();
                }

            }

            // From B to A
            if (direction == Direction.B_A) {
                if ((x == 5 && y == 11) || (x == 4 && y == 13)) {   //Acquire track_B
                    singleTrack("track_B");
                }

                if (x == 1 && y == 10) { // Release Lower Stations & Acquire next Track [Double Track]
                    semRel("stat_B1"); semRel("stat_B2");
                    multiTrack("track_B1", "track_B2");
                }

                if ((x == 9 && y == 9) || (x == 9 && y == 10)) { // Release track_B & Acquire track_A
                    semRel("track_B");
                    singleTrack("track_A");
                }

                if ((x == 19 && y == 8)) {  // Release double track & Acquire Higher Stations
                    semRel("track_B1"); semRel("track_B2");
                    multiTrack("stat_A1", "stat_A2");
                }

                if ((x == 12 && y == 7) || (x == 12 && y == 8)) {  // Release track_A & Acquire crossing
                    semRel("track_A");
                    //singleTrack("crossing");
                    theCrossing();
                }

                if ((x == 6 && y == 7) || (x == 8 && y == 5)) {  // Release crossing
                    semRel("crossing");
                }

                if ((x == 14 && y == 3) || (x == 14 && y == 5)) { // Stop at Higher Stations & stop protocol
                    stopProtocol();
                }


            }
        }
    }


    // Methods for Acquring tracks & calling the necessary functions for action
    private void multiTrack (String semName1, String semName2) throws CommandException {
        quickStop();
        if (!semAcq(semName1)) {     // If can't acquire upper track
            semAcq(semName2);       //  Acquire Lower Track
            switchSet(semName2);
            quickStart();
            //return semName2;
        } else {
        switchSet(semName1);
        quickStart();
        //return semName1;
        }
    }
    private void singleTrack (String semName) throws CommandException {
        quickStop();
        //while (!semAcq(semName))
        //  sleep(250);
        semAcqUninterruptibly(semName);   // Blocks and waits if Semaphore is taken, until available again
        switchSet(semName);
        quickStart();
    }
    private void theCrossing () throws CommandException {
        quickStop();
        //while (!semAcq("crossing"))
        //  sleep(250);
        semAcqUninterruptibly("crossing");   // Blocks and waits if Semaphore is taken, until available again
        quickStart();
    }

    // Methods for setting switches based on Direction
    private void switchSet (String semName) throws CommandException {
        if (direction == Direction.A_B)
            switch4AB(semName);  // Handles switches when going from A (upper stations) to B (lower stations)
        else
            switch4BA(semName);  //Handles switches when going from B to A
    }
    private void switch4AB (String semName) throws CommandException {
        // Sets switches based on the next track semaphore you acquired &
        // The last sensor you affected
        if (semName.equals("track_A")) {
        if (lastX == 12 && lastY == 7)
            tsi.setSwitch(17, 7, 0);
        if (lastX == 12 && lastY == 8)
            tsi.setSwitch(17, 7, 1);
        }

        if (semName.equals("track_B1"))
            tsi.setSwitch(15, 9, 0);
        if (semName.equals("track_B2"))
            tsi.setSwitch(15, 9, 1);

        if (semName.equals("track_B")) {
            if (lastX == 9 && lastY == 9)
                tsi.setSwitch(4, 9, 1);
            if (lastX == 9 && lastY == 10)
                tsi.setSwitch(4, 9, 0);
        }

        if (semName.equals("stat_B1"))
            tsi.setSwitch(3, 11, 1);
        if (semName.equals("stat_B2"))
            tsi.setSwitch(3, 11, 0);

    }
    private void switch4BA (String semName) throws CommandException {
        // Sets switches based on the next track semaphore you acquired &
        // The last sensor you affected
        if (semName.equals("track_B")) {
            if (lastX == 5 && lastY == 11)
                tsi.setSwitch(3, 11, 1);
            if (lastX == 4 && lastY == 13)
                tsi.setSwitch(3, 11, 0);
        }

        if (semName.equals("track_B1"))
            tsi.setSwitch(4, 9, 1);
        if (semName.equals("track_B2"))
            tsi.setSwitch(4, 9, 0);

        if (semName.equals("track_A")) {
            if (lastX == 9 && lastY == 9)
                tsi.setSwitch(15, 9, 0);
            if (lastX == 9 && lastY == 10)
                tsi.setSwitch(15, 9, 1);
        }

        if (semName.equals("stat_A1"))
            tsi.setSwitch(17, 7, 0);
        if (semName.equals("stat_A2"))
            tsi.setSwitch(17, 7, 1);
    }

    // Methods for Stopping & resuming the train
    private void quickStart () throws CommandException { tsi.setSpeed(this.id, this.speed);}
    private void quickStop () throws CommandException { tsi.setSpeed(this.id, 0);}

    // Methods for Starting & Stopping on stations
    private void startProtocol () throws CommandException {
        quickStart();
        System.out.println("" + this.id + " Started" );
    }
    private void stopProtocol () throws CommandException, InterruptedException {
        quickStop();
        System.out.println("" + this.id + " Stopped" );

        int delay = 1000 + (20 * Math.abs(this.speed));
        this.speed = this.speed * -1;
        sleep(delay);

        switchDir();     // Switches to new Direction
        startProtocol(); // starts the train back up after sleep
    }

    // Methods for setting initial start direction & Switching direction at stations
    private void initalDir () {
        if ( (this.startStation.equals("stat_A1")) || (this.startStation.equals("stat_A2")) ) {
            this.direction = Direction.A_B;
        }
        else {
            this.direction = Direction.B_A;
        }
    }
    private void switchDir () {
        if (this.direction == Direction.A_B)
            this.direction = Direction.B_A;
        else
            this.direction = Direction.A_B;
    }

    // Methods for Acquiring & Releasing Semaphores, also special blocking wait method for waiting on a semaphore
    private void semAcqUninterruptibly (String semName) {
        semaphores.get(semName).acquireUninterruptibly(); // Blocks and Waits until Semaphore is available
        System.out.println("" + semName + " acquired");
        mySemaphores.add(semaphores.get(semName)); // test
    }
    private boolean semAcq (String semName) {
        if (semaphores.get(semName).tryAcquire()) {
            System.out.println("" + semName + " acquired");
            mySemaphores.add(semaphores.get(semName)); // test
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
        if (!mySemaphores.contains(sem)) // Make sure I can only release Semaphores I'm holding
            return false;
            sem.release();
        mySemaphores.remove(sem); // Remove from my collection of current semaphores I'm holding
        System.out.println("" + semName + " released");
        return true;
    }
}

 

