using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Linq;


public class Agent
{
    public Coordinates coords { get; set; }
    public int nbGold { get; set; }
    public int nbArrow { get; set; }

    public GameObject agentTile;

    // public Stack<Coordinates> prevCoords = new Stack<Coordinates>();

    public Agent(Coordinates startCoords, int arrowsAtStart)
    {
        coords = startCoords;
        nbGold = 0;
        nbArrow = arrowsAtStart;
    }

    public void MoveAgent(Coordinates newCoords)
    {
        coords.col = newCoords.col;
        coords.row = newCoords.row;
    }
}

