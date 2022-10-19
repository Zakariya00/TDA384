package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver {

    // Shared variables and states
    static private AtomicBoolean found = new AtomicBoolean(false);
    static private ConcurrentSkipListSet<Integer> visited = new ConcurrentSkipListSet<>();
    private List<ForkJoinSolver> forkedSolvers = new ArrayList<>();


    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    private ForkJoinSolver(int start, Maze maze) {
        this(maze);
        this.start = start;
    }




    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        List<Integer> result = null;
        int player = maze.newPlayer(start); // one player active on the maze at start
        frontier.push(start); // start with start node

        // as long as not all nodes have been processed AND path not found yet
        while (!frontier.empty() && !found.get()) {
            int current = frontier.pop(); // get the new node to process

                // if current node has a goal
            if (maze.hasGoal(current)) {
                maze.move(player, current); // move player to goal
                found.set(true); // set found to true
                return pathFromTo(start, current); // search finished: reconstruct and return path
            }

            if (visited.add(current)) {
                maze.move(player, current); // move player to current node

                ArrayList<Integer> neighbours = getNeighbours(current); // get adjacent unvisited nodes
                 for (int i = 0; i < neighbours.size(); i++) {
                     int neighbour = neighbours.get(i);
                     predecessor.put(neighbour, current);

                     if (i == 0) frontier.push(neighbour);
                     else
                         forkNew(neighbour);
                    }
            }
        }

        // all nodes explored, no goal found
        result = forkJoin();
        //return null;
        return result;
    }

    private void forkNew(int currNode) {
        if (!visited.contains(currNode)) {
            ForkJoinSolver forked = new ForkJoinSolver(currNode, maze);
            forkedSolvers.add(forked);
            forked.fork();
        }
    }

    private List<Integer> forkJoin() {
        List<Integer> pathfromto = null;
        for (ForkJoinSolver forked : forkedSolvers) {
            List<Integer> path = forked.join();
            if (path != null) {
                int middle = path.remove(0);
                pathfromto = pathFromTo(start, middle);
                pathfromto.addAll(path);
            }
        }
        return pathfromto;
    }

    private ArrayList<Integer> getNeighbours(int currNode) {
        ArrayList<Integer> neighbours = new ArrayList<>();
        // for every node neighbour adjacent to current
        for (int neighbour : maze.neighbors(currNode)) {
            if (!visited.contains(neighbour)) {  // if neighbour has not been already visited,
                neighbours.add(neighbour);
            }
        }
        return neighbours;
    }

}
