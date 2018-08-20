using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FindPath : MonoBehaviour
{
    public Grid grid; // this is the unity grid object. It is only used to translate grid coordinates to global coordinates or the other way round.
    public NavGrid navGrid; // this is the actual grid used for navigation. It's based on the Unity grid, but any collection of nodes would do.
    public Actor actor; // the actor used for pathing
    public List<Vector3Int> totalPath; // the actual path
    private string pathMessage; // just a string used for printing the path to the console

    public void A_Star(Vector3Int start, Vector3Int goal)
    {

        /*
         * What does "undiscovered" mean?
         * An undiscovered node is not known to the algorithm yet. All nodes can be found in the navGrid.cells list. When a node was discovered (the goal node
         * starts marked as discovered), the algorithm looks for neighboring nodes and if there are any that are marked as walkable, they are discovered.
         * 
         * What does "discovered" mean?
         * A discovered node means that we know the node's location and know it is walkable, but do not know the cost of of getting there from the goal node (gScore)
         * or the cost of getting from goal to start by passing that node (fScore). A discovered node is part of openSet.
         * 
         * What das "discovered and evaluated" mean?
         * If a node was discovered and evaluated, we do now its location as well as its fScore and gScore. It is part of the closedSet now.
         * 
         * How is the cost (fScore and gScore) calculated?
         * The cost of travelling across a node is based on the most direct path ("beeline") from goal to the node (gScore) or
         * from goal to the node + from the node to start (fScore). Visually speaking, the algorithm first tries to path directly towards the goal. When
         * hitting an obstacle, it tries to go around by staying as close (again, based on direct path) to the goal as possible. If tiles are used that modify
         * the walking speed of an actor, the algorithm is able to take that into calculation if that modifier is added to a node's gScore. In this example,
         * I don't want actors to be that clever, but it's up to you.
         */

        List<Vector3Int> closedSet = new List<Vector3Int>(); // set of nodes that have already been evaluated
        List<Vector3Int> openSet = new List<Vector3Int>(); // set of nodes that have been discovered, but not yet evaluated
        // In the beginning, only the goal node is known. A* returns the path beginning at the end, so the algorithm is started with goal instead of start.
        // That way, the path does not need to be resorted in the end.
        // gScore of a node describes the cost of getting from goal to the node. This is where the walkCost of a tile also needs to be anticipated.
        // g(current) = distance between goal and current + walkCost(current)
        // gScore[current] = Vector3.Distance(grid.GetCellCenterWorld(goal), grid.GetCellCenterWorld(current))
        Dictionary<Vector3Int, float> gScore = new Dictionary<Vector3Int, float>();
        // fScore of a node describes the cost of getting from goal to start by passing this node.
        // f(current) = distance between start and current + g(current)
        // fScore[current] = Vector3.Distance(grid.GetCellCenterWorld(start), grid.getCellCenterWorld(current)) + gScore[current]
        Dictionary<Vector3Int, float> fScore = new Dictionary<Vector3Int, float>();
        // the "cameFrom" node is the neighboring node through which a node can be reached from most efficiently. When the pathfinding is completed,
        // this leaves the one path from goal to start that is the most efficient solution based on the nodes' fScores.
        Dictionary<Vector3Int, Vector3Int> cameFrom = new Dictionary<Vector3Int, Vector3Int>();

        // start with adding goal to openSet and setting gScore and fScore to 0 for goal (getting from goal to goal has no cost)
        openSet.Add(goal); gScore[goal] = 0f; fScore[goal] = 0f;

        // start the main loop. This loops as long as there are still nodes that have been discovered and not evaluated yet.
        // In the beginning, it only contains the goal node. During the loop, the neighbors of each node in openSet are being looked at and added to the queue
        // ("discovered"), if they have not yet been discovered (openSet) or evaluated (closedSet).
        bool pathFound = new bool(); pathFound = false; // this is used for debugging (see below while loop)
        while (openSet.Count > 0)
        {

            // which node has the lowest fScore in openSet?
            float fScoreMin = new float();
            Vector3Int nodeWithSmallestFScore = new Vector3Int();
            foreach (Vector3Int node in openSet)
            {

                if (node == openSet[0]) // if first node in openSet
                {

                    fScoreMin = fScore[node]; // set fScoreMin to this node's fScore and
                    nodeWithSmallestFScore = node; // remember this node, because it's the node with the smallest currently known fScore

                }
                else if ( node != openSet[0] && fScore[node] < fScoreMin) // if not the first node in openSet, but its fScore is smaller than fScoreMin
                {

                    fScoreMin = fScore[node]; // set fScoreMin to this node's fScore and
                    nodeWithSmallestFScore = node; // remember this node, because it's the node with the smallest currently known fScore

                }

            }

            // now we know the node with the smallest fScore in openSet
            if (nodeWithSmallestFScore == start) // Is this node the start node?
            {

                Debug.Log("Actor " + actor.data.actorName + " has successfully pathed from cell " + start.x + ":" + start.y + " to cell " + goal.x + ":" + goal.y + " via:");
                pathFound = true;
                ReconstructPath(cameFrom, nodeWithSmallestFScore); // then we can start reconstructing the most efficient path
                break; // and break the while loop

            }
            else // if not
            {

                // we move the node from openSet to closedSet (thus mark it as "discovered and evaluated"). This is important to prevent cycles.
                openSet.Remove(nodeWithSmallestFScore);
                closedSet.Add(nodeWithSmallestFScore);

                // Now we look for neighboring nodes. In this example, actors can move vertically, horizontally and diagonally on the grid.
                // This means that eachnode has a maximum of 8 possible neighbors.
                for (int x = nodeWithSmallestFScore.x - 1; x <= nodeWithSmallestFScore.x + 1; x++)
                {

                    for (int y = nodeWithSmallestFScore.y - 1; y <= nodeWithSmallestFScore.y + 1; y++)
                    {

                        // we have to look up the the cell x:y in the navGrid.cells list to check if it can act as a valid and walkable node.
                        foreach(CellData neighbor in navGrid.cells)
                        {

                            // if the cell exists in navGrid.cells, is marked as walkable AND has not been evaluated yet
                            if (neighbor.position == new Vector3Int(x,y,0) && neighbor.walkable == true && closedSet.Contains(neighbor.position) == false)
                            {

                                // we now calculate its tentative gScore
                                float tentative_gScore = new float();
                                tentative_gScore = CalculateGScore(gScore[nodeWithSmallestFScore], goal, neighbor.position);

                                // and check if that node has already been discovered
                                if (openSet.Contains(neighbor.position) == false)
                                {

                                    // if not, we add it to openSet, set its gScore to tentative_gScore and calculate its fScore
                                    // Note that dictionary.Add() throws an exception if it tries to add a duplicate key (which shouldn't happen)
                                    openSet.Add(neighbor.position);
                                    gScore.Add(neighbor.position, tentative_gScore);
                                    fScore.Add(neighbor.position, CalculateFScore(start, neighbor.position, gScore[neighbor.position]));

                                    // this is the best path yet. Remember it!
                                    cameFrom.Add(neighbor.position, nodeWithSmallestFScore);

                                }
                                else
                                {

                                    // if the node has already been discovered, we have to check if the tentative gScore is smaller than its current gScore
                                    if (tentative_gScore < gScore[neighbor.position])
                                    {

                                        // and if it is, we have to reset its gScore, recalculate its fScore
                                        gScore[neighbor.position] = tentative_gScore;
                                        fScore[neighbor.position] = CalculateFScore(start, neighbor.position, gScore[neighbor.position]);

                                        // this is the best path yet. Remember it!
                                        cameFrom[neighbor.position] = nodeWithSmallestFScore;

                                    }

                                }

                            }

                        }

                    }

                }

                // this is pretty much it. The while loop will continue to look for undiscovered neighbors of each node that was previously discovered. It
                // will do so by prioritizing nodes with the minimal fScores. The loop will end if there are no undiscovered nodes left (every neighbor is
                // already part of openSet or closedSet) or every discovered node has already been evaluated (openSet is empty), which means that there is no
                // valid path from goal to start. It will break as soon as a path was found from goal to start.
                if (pathFound == false) Debug.Log("Actor " + actor.data.actorName + " could not find a path from cell " + start.x + ":" + start.y + " to cell " + goal.x + ":"  + goal.y + ".");

            }

        }

    }

    void ReconstructPath(Dictionary<Vector3Int, Vector3Int> cameFrom, Vector3Int current)
    {
        /*
         * This function builds a complete path from the node-cameFrom-node-combinations that were previously constructed
         * by the A_Star algorithm.
         */

        pathMessage = "";
        while (cameFrom.ContainsKey(current))
        {

            current = cameFrom[current]; // go to previous node and
            totalPath.Add(current); // add it to final path

            // build pathMessage
            pathMessage = pathMessage + current.x + ":" + current.y;
            if (cameFrom.Keys.Count > 1) pathMessage = pathMessage + ", ";

            // show path by painting nodes in green color
            foreach (CellData cell in navGrid.cells)
            {

                if (cell.position == current) cell.node.GetComponent<SpriteRenderer>().color = Color.green;

            }

        }

        Debug.Log(pathMessage); // print path
    }

    float CalculateGScore(float gScoreOfPreviousNode, Vector3Int goal, Vector3Int current)
    {

        // This is where the gScore is calculated (see documentation). moveCosts of tiles (e.g. slowing effects by water tiles) go here if needed.
        float ret = Vector3.Distance(grid.GetCellCenterWorld(goal), grid.GetCellCenterWorld(current)) + gScoreOfPreviousNode;
        return (ret);

    }

    float CalculateFScore(Vector3Int start, Vector3Int current, float gScore_current)
    {

        // This is where the fScore is calculated (see documentation).
        float ret = Vector3.Distance(grid.GetCellCenterWorld(start), grid.GetCellCenterWorld(current)) + gScore_current;
        return (ret);

    }

}
