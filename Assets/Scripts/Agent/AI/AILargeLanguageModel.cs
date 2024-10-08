using UnityEngine;

namespace Agent.AI
{
    public class AILargeLanguageModel : AIBasic
    {
        // public override async void PlayTurn()
        public override void PlayTurn()
        {

            Debug.Log("test");
            /*
            You are an agent in a grid-based game that involves navigating through a cave to find a piece of gold while avoiding pits. Here are the detailed and exhaustive rules of the game:
            Game Objective: The objective of the game is to find the gold without dying and losing the game. Once you have picked the gold, move back to the starting cell to win.
            Game Setup: The location of the gold and the pits is unknown.
                You start at a fixed location of coordinates (1,1) and can move one cell at a time in any of these four directions (right, left, up or down). 
            Pits: If the you are in the same cell as a pit, you die and you lose the game. Cells with a pit are surrounded by cells with a breeze. If there is a breeze in your cell, it means that a pit is in an adjacent cell of your current cell. 
            Gold: The gold is randomly placed somewhere in the cave. If you move into a cell containing the gold, you can pick it up. 
            Actions: The player can take one of four actions at any time: move right, move left, move up, move down and pick up the gold.
            Sensing: You only know what is in the current cell and the cells you have visited already. It is better to not make risky move and be sure of moving to a safe cell.
            
            Here is an example of a previous game:
            "- You are in  cell (1,1): There is no breeze, no pit, and no gold in this cell. What is your next action? 
            - Move right -- cell(2, 1) is safe since there is no breeze in adjacent (1, 1) cell. Moving down was another valid option.
            - Moving right. You are now in cell (2, 1).  There is a breeze, no pit, and no gold in this cell. What is your next action? 
            - Move left - adjacent cells are not safe since the is a breeze in the current cell. Only the left cell (1, 1) Is safe since I visited it earlier and there were no breeze nor pit."
                        
            New Game, you must not lose and find the gold.
            You are in  cell (1,1): There is no breeze, no pit, and no gold in this cell. What is your next action? Answer with the format: Action - Reason for doing the action.
            
            
            Moving right. You are now in cell (2, 1). There is no breeze, no pit, and no gold in this cell. What is your next action? 
            
            Moving up. Unfortunately, there is a wall in this cell. You can't move up.  
            You go back to cell (2, 1). There is no breeze, no pit, and no gold in this cell. What is your next action?
            
            Generate a 4x4 grid of cells with a pit and a gold randomly placed in the grid. The starting cell is (1,1).
               1   2   3   4  
            1| X |   |   |   |
            -----------------
            2|   |   | B |   |
            -----------------
            3|   | B | P | B |
            -----------------
            4| G |   | B |   |
               
            */

            // var api = new OpenAIAPI(await System.IO.File.ReadAllTextAsync("gpt_key.txt"));
            // var chat = api.Chat.CreateConversation();
            // chat.AppendSystemMessage(
            //     "You are in a grid-based game that involves navigating through a cave to find a piece of gold while avoiding pits. Here are the detailed and exhaustive rules of the game:" +
            //     "Game Setup: The game is played on an 4 x 4 grid of cells. The location of the gold, pits is randomly determined for each new game. " +
            //     "Game Objective: The objective of the game is to find the gold and then leave the cave without dying. " +
            //     "Player: You start at a fixed location (1,1) and can move one cell at a time in any of the four cardinal directions (north, south, east, or west), you are allowed go back to previous cells, one cell at a time. " +
            //     "Pits:  If the you are in the same cell as a pit, you die and the game ends immediately. If there is a breeze in your cell, it means that a pit is in an adjacent cell of your current cell. " +
            //     "Gold: The gold is randomly placed somewhere in the cave. If you move into a cell containing the gold, you pick it up. " +
            //     "Actions: The player can take one of four actions at any time: move south, north, east or west. " +
            //     "Sensing: You only know what is in the current cell and the cells you have visited already. It is better to not make risky move and be sure of moving to a safe cell. Since you can only sense pits in adjacent cells, you shall not move to a cell that has a breeze unless you are sure that there is no pit in the adjacent cell. " +
            //     "There is no breeze nor gold in the starting cell. Make sure not to die.");

            // // give a few examples as user and assistant
            // chat.AppendUserInput("You are now in (1, 1). This is the starting cell. There is no breeze nor gold in the cell. What is your next move");
            // chat.AppendExampleChatbotOutput("north --- The north cell is unexplored, and safe since there is no breeze in my current cell.");
            // chat.AppendUserInput("You are now in (1, 2). There is no breeze nor gold in the cell. What is your next move");
            // chat.AppendExampleChatbotOutput("north --- The north cell is unexplored, and safe since there is no breeze in my current cell.");

            // // now let's ask it a question'
            // chat.AppendUserInput("You are now in (1, 1). This is the starting cell. There is no breeze nor gold in the cell. What is your next move");
            // // and get the response
            // var response = await chat.GetResponseFromChatbotAsync();
            // Debug.Log(response); // "Yes"
        }
    }
}