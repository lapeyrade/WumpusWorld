using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;


public class Agent
{
    public Coordinates coords { get; set; }
    public int golds { get; set; }
    public int arrows { get; set; }

    public GameObject agentTile;

    public Stack<Coordinates> prevCoords = new Stack<Coordinates>();

    public Agent(Coordinates startCoords, int arrowsAtStart)
    {
        coords = startCoords;
        golds = 0;
        arrows = arrowsAtStart;
        prevCoords.Push(startCoords);
    }
}

