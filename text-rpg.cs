using System;
using System.Collections.Generic;

public class Program
{
	public static void Main()
	{
		// Let's start the game! We'll initialize the gameController that will be storing everything.
		GameController gameController = new GameController();
		// Create a random number for the gnoll spawns as well as a List to store the gnolls
		Console.WriteLine("You're venturing deep into the forest when suddenly...");
		Random r = new Random();
		int numberOfGNolls = r.Next(2, 5);
		for (int i = 0; i < numberOfGNolls; i++)
		{
			Gnoll newGnoll = new Gnoll();
			gameController.gnolls.Add(new Gnoll());
		}
		Console.WriteLine("A group of " + gameController.gnolls.Count  + " wild gnolls appears!");
	}
}

class GameController
{
	public Hero hero;
	public List<Gnoll> gnolls = new List<Gnoll>();
	public GameController()
	{
		Console.WriteLine("Game started. Please name your hero:");
	// Create a hero.
	string heroName = Console.ReadLine();
	this.hero = new Hero(heroName);
	}
}

class Hero
{
	public string name;
	public Hero(string name)
	{
		this.name = name;
		Console.WriteLine("Welcome, " + name + "!");
	}
}

class Gnoll
{
	public Guid id;
	public int hpTotal = 50;
	private int hpCurrent;
	public Gnoll()
	{
		// Create a unique id (a 'guid') for this gnoll so we can access it all times
		// This doesn't need to be done in Unity! Unity generates unique ids for every Object instance:
		// Object.GetInstanceID() https://docs.unity3d.com/ScriptReference/Object.GetInstanceID.html?from=MonoBehaviour
		this.id = Guid.NewGuid();
		// Set the current hp to total hp
		this.hpCurrent = hpTotal;
	}
}
