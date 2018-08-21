unit Unit1;

{$mode objfpc}{$H+}

// This program is an attempt to simulate reproduction, dispersal and evolution
// of plants within a fragmented landscape under the influece of disturbances
// considering a seed size/seed number trade-off. We are assuming that a seed's
// success of copempeting with other seeds for a suitable location will largely
// depend on its size. However, assuming that a plant has a limited amount of
// ressources available for producing seeds, the bigger its seeds are, the less
// it can produce (seed size = 1 / number of seeds).

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TAMultiSeries, Forms, Math, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  TPatch = record // plant template
         occupied : boolean; // is this patch occupied by a plant?
         disturbed : boolean; // will this patch die due do disturbances?
         seedAmountExpected : double; // expected amount of seeds
         seedAmountReal : integer; // real amount of seeds per timestep
         competitors : array[1..1000] of double; // array of competitors (seedSize)
         competitorsAmount : integer; // amount of competitors
         number_disturbances : integer; // number of disturbances on this patch
         t_last_disturbance : integer; // time since last disturbance
  end;

  { TForm1 }

  TForm1 = class(TForm)
    betaEdit: TLabeledEdit;
    drawCheckBox: TCheckBox;
    disturbanceCheckBox: TCheckBox;
    disturbanceEdit: TLabeledEdit;
    debug: TMemo;
    dispersalEdit: TLabeledEdit;
    landscapeCheckBox: TCheckBox;
    mutationStrengthEdit: TLabeledEdit;
    maxSeedsEdit: TLabeledEdit;
    numbersChart: TChart;
    numbersSeries: TLineSeries;
    chartsPageControl: TPageControl;
    paraGroupBox: TGroupBox;
    mortalityEdit: TLabeledEdit;
    mutationRateEdit: TLabeledEdit;
    nStartEdit: TLabeledEdit;
    rCurrentEdit: TLabeledEdit;
    saveCheckBox: TCheckBox;
    runButton: TButton;
    exitButton: TButton;
    patchSaveCheckBox: TCheckBox;
    SaveDialog: TSaveDialog;
    rMaxEdit: TLabeledEdit;
    patchSaveDialog: TSaveDialog;
    Shape1: TShape;
    tabNumbers: TTabSheet;
    tDelEdit: TLabeledEdit;
    tMaxEdit: TLabeledEdit;
    procedure landscapeCheckBoxChange(Sender: TObject);
    procedure disturbanceCheckBoxChange(Sender: TObject);
    procedure drawCheckBoxChange(Sender: TObject);
    procedure exitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure patchSaveCheckBoxChange(Sender: TObject);
    procedure runButtonClick(Sender: TObject);
    procedure saveCheckBoxChange(Sender: TObject);
    procedure tDelEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
    PMAX = 10000; // max population size
    XMAX = 128; // landscape size
    YMAX = 128; // landscape size

var
  Form1: TForm1;
  outputfile : textfile; // file for saving global data
  filename : string; // location and name of global save file
  inputfile : textfile;
  inputfilename : string; // location and name of the disturbance landscape file
  patchfilename : string; // location and name of patch save file
  patchoutputfile : textfile; // file for saving patch-specific data
  tDel : integer; // simulation delay (miliseconds)
  tMax : integer; // max time
  rMax : integer; // max number of replications
  MAXSEEDS : integer; // max amount of seeds
  p_disturbance : double; // probability of disturbance

  n_start : integer; // number of plants at beginning
  n_current : integer; // current amount of plants
  n_max : integer; // maximum amount of available patches
  landscape : array [1..XMAX, 1..YMAX] of TPatch; // population with PMAX patches

  beta : double; // p_col = seedSize^beta
  mortality : double; // standard mortality rate (per timestep), or probability of death
  mutationRate : double; // rate of mutation (number of seeds)
  mutationStrength : double; // strength of mutation
  dispersalRange : integer; // maximum range of dispersal

  draw_checked : boolean; // should the program draw the landscape?
  disturbance_checked : boolean; // should there be disturbances?
  constantLandscape_checked : boolean; // should the disturbance landscape be constant?

implementation

{$R *.lfm}

{ TForm1 }

///////////
// AWAKE //
///////////
procedure TForm1.FormCreate(Sender: TObject);

// this procedure is being called when the program is executed

var
  code : integer; // debugging index number
  yRange : double; // data chart y axis range
begin

     // initialize booleans
     disturbance_checked := Form1.disturbanceCheckBox.Checked;
     draw_checked := Form1.drawCheckBox.Checked;

     // initialize data chart
     val(Form1.maxSeedsEdit.Text, yRange, code);
     Form1.numbersChart.AxisList.LeftAxis.Range.Max := yRange;

     // initialize occupied patches (N_start)
     n_max := XMAX * YMAX;
     Form1.nStartEdit.Text := n_max.ToString();

end;


{ runtime procedures }

//////////////////////////
// INITIALIZE LANDSCAPE //
//////////////////////////
procedure InitializeLandscape(const x : integer; const y : integer);

// When this procedure is being called, an empty landscape with two dimensions
// (x,y) is initialized.

begin

     with landscape[x,y] do
     begin

          occupied := FALSE;
          disturbed := FALSE;
          seedAmountExpected := 0;
          seedAmountReal := 0;
          number_disturbances := 0;
          t_last_disturbance := 0;

     end;

end;

/////////////////////////
// READ LANDSCAPE FILE //
/////////////////////////
procedure ReadLandscape (const x : integer; const y : integer);

// during this procedure, the landscape file is read. The landscape file must
// have n_max characters (0 or 1). When read, a patch will become either
// undisturbed (0) or disturbed (1).

var
  c : integer; // 1 = disturbed patch, 0 = undisturbed patch

begin

     with landscape[x,y] do
     begin

          Read(inputfile, c);
          if c = 1 then disturbed := TRUE
          else disturbed := FALSE;

     end;

end;

///////////////////////////
// INITIALIZE POPULATION //
///////////////////////////
procedure InitializePopulation();

// During this procedure, the landscape which was initialized earlier gets
// populated by n_start plants at random coordinates.

var
  x, y : integer; // coordinates

begin

     // pick random patches until one is not occupied
     repeat

           x := random (XMAX) +1;
           y := random (YMAX) +1;

     until not landscape[x,y].occupied;

     // occupy this patch
     with landscape[x,y] do
     begin

          occupied := TRUE;
          seedAmountExpected := random() * MAXSEEDS;

     end;

end;

/////////////////
// DISTURBANCE //
/////////////////
procedure Disturbance (const x : integer; const y : integer);

// This procedure is being called during disturbances and checks if a patch is
// disturbed. If it is, any plant occupying this patch will die.

begin

     // check for death according to landscape and disturbances
     with landscape[x,y] do
     begin

          if disturbed then
          begin

               occupied := FALSE;
               inc(number_disturbances);
               t_last_disturbance := 0;

          end;

     end;

end;

///////////
// DEATH //
///////////
procedure Death(const x : integer; const y : integer);

// This procedure is being called every time step. It checks for random
// (natural) mortality. If random() < mortality, the plant will die.

begin

     // check for death according to standard mortality
     with landscape[x,y] do
     begin

          if random() < mortality then occupied := FALSE;

     end;

end;

///////////////////
// REPRODUCTION //
///////////////////
procedure Reproduction (const x : integer; const y : integer);

// This procedure is being called every time step. Occupied patches will
// produce a random amount of seeds ('seedAmountReal', poission distribution
// using 'seedAmountExpected' as an average).

var
  a, b : double;
  i : integer;
begin

     with landscape[x,y] do
     begin

          if occupied then
          begin

               // produce a random amount of seeds considering the expected seed amount for this plant (poisson)
               a := Exp(-seedAmountExpected);
               b := 1;
               i := -1;
               repeat

                inc(i);
                b := b * random();

                until b < a;
                seedAmountReal := i;

          end;

          // empty patches don't produce any seeds
          if not occupied then seedAmountReal := 0;

     end;

end;

//////////////
// DISPERAL //
//////////////
procedure Dispersal(const x : integer; const y : integer);

// This procedure is being called every time step. For every seed that was
// produced earlier (see procedure Reproduction), a random target patch will
// be chosen (within 'dispersalRange'). A torus effect might be applied.
// The seed will then be given its parent's trait 'seedAmountExpected', which
// might mutate ('mutationRate', 'mutationStrength). Finally, the seed will be
// written to the 'competitors' array of the targeted patch.

var
  s : integer; // seed index number
  xStart, yStart : double; // starting coordinates within parent's patch
  xTarget, yTarget : integer; // target patch coordinates
  seed : double; // local seed variable

begin


     with landscape[x,y] do
     begin

          // do for every seed
          for s := 1 to seedAmountReal do
          begin

               // make starting point of seed random within parent's patch
               xStart := x + (random - 0.5);
               yStart := y + (random - 0.5);

               // disperse seed randomly
               xTarget := round(xStart + 2 * (random - 0.5) * dispersalRange);
               yTarget := round(yStart + 2 * (random - 0.5) * dispersalRange);

               // apply torus effect
               if xTarget > XMAX then xTarget := xTarget - XMAX;
               if xTarget < 1 then xTarget := xTarget + XMAX;
               if yTarget > YMAX then yTarget := yTarget - YMAX;
               if yTarget < 1 then yTarget := yTarget + YMAX;

               // calculate Parent's seedSize, save value to seed and check for mutation
               seed := 1 / seedAmountExpected;
               if random() < mutationRate then
               begin

                    // mutate seedsize
                    seed := (random() - 0.5) * mutationStrength + seed;
                    if seed < ( 1 / MAXSEEDS) then seed := 1 / MAXSEEDS;
                    if seed > 1 then seed := 1;

               end;

               // save seed to the target's competitors array
               inc(landscape[xTarget,yTarget].competitorsAmount);
               landscape[xTarget,yTarget].competitors[(landscape[xTarget,yTarget].competitorsAmount)] := seed;

          end;

     end;

end;

////////////
// GROWTH //
////////////
procedure Growth(const x : integer; const y : integer);

// This procedure is being called every time step. If this patch is already
// occupied, the 'competitors' patch will be emptied. If this patch is not
// occupied, a lottery for chosing a 'winner' seed will run. A single seed's
// probability of occupying this patch will depend on its size.

var
  c : integer; // competitors index number
  p_col_total : double; // local total probability of colonisation
  z, r : double;
  winner : integer; // winner

begin

     with landscape[x,y] do begin

          // if occupied then delete competing seeds
          if occupied then competitorsAmount := 0;

          // if empty and seeds present then begin lottery
          if not occupied and (competitorsAmount > 0) then
          begin

               // initialize
               p_col_total := 0;
               winner := -1;
               z := 0;

               // calculate p_col for every competitor
               for c := 1 to competitorsAmount do
               begin

                    p_col_total := p_col_total + (competitors[c] ** beta);

               end;

               // choose winning competitor
               r := random() * p_col_total;
               for c := 1 to competitorsAmount do
               begin

                    z := z + (competitors[c] ** beta);
                    if z > r then
                    begin

                         winner := c;
                         break;

                    end;

               end;

               // occupy this patch
               occupied := TRUE;
               seedAmountExpected := 1 / competitors[winner];
               competitorsAmount := 0;

          end;

     end;

end;

//////////
// DRAW //
//////////
procedure Draw(const x : integer; const y : integer);

// This procedure is being called every time step if the 'Draw' check box is checked.

var
  drawXMAX, drawYMAX : double; // percentage of form size that sould be used for drawing
  xSize, ySize : double; // size of one patch
begin

     drawXMAX := 0.5 * Form1.Width;
     drawYMAX := 1.0 * Form1.Height;
     xSize := drawXMAX / XMAX;
     ySize := drawYMAX / YMAX;

     with landscape[x,y] do
     begin

          // draw landscape
          if occupied then
          begin

               Form1.Canvas.Brush.Color := RGBToColor(0, round(seedAmountExpected/MAXSEEDS * 255), 0);
               Form1.Canvas.Rectangle (
                                 round((x) * xSize),
                                 round((y) * ySize),
                                 round((x + 1) * xSize),
                                 round((y + 1) * ySize)
                                 );

          end

          else
          begin

               Form1.Canvas.Brush.Color := RGBToColor(255, 255, 255);
               Form1.Canvas.Rectangle (
                                 round((x) * xSize),
                                 round((y) * ySize),
                                 round((x + 1) * xSize),
                                 round((y + 1) * ySize)
                                 );

          end;

     end;

end;

////////////////
// SIMULATION //
////////////////
procedure Simulation();

// this is kind of the 'main' procedure. Every other run-time procedure is being
// called in here. This procedure basically consists of three parts:
// --- Initialization ---
// during initialization, all parameters will be initialized (according to the
// values read from all the input fields. All replications will be run using
// the same parameters
// --- Replication Loop ---
// every replication will randomly pick a landscape file and then start the simulation
// --- Time Step Loop ---
// for every time step (t = 1 to tMax), the landscape different procedures are
// called, (e.g. for reproduction and dispersal). One time step is supposed to
// symbolize one year or reproductive season

var
  code : integer; // debugging index number
  r : integer; // replication index number (replication loop)
  t : integer; // time index number (time step loop)
  x, y : integer; // x and y coordinates (for looping procedures)
  i : integer; // initialization index number (for initializing the landscape)
  totalSeedAmount : double; // total seed amount for meanSeedAmount calculation
  meanSeedAmount : double; // mean seed amount
  occupiedAmount : integer; // amount of tabOccupied patches

begin


     { Initialization }

     // if 'Sava Global Data' is checked then assign and open 'outputfile' for writing
     if Form1.saveCheckBox.Checked then
     begin

          AssignFile(outputfile, filename);
          Append(outputfile);

     end;

     // read editable parameters and initialize kind of everything
     val(Form1.tMaxEdit.Text, tMax, code); // tMax
     val(Form1.tDelEdit.Text, tDel, code); // tDel
     val(Form1.rMaxEdit.Text, rMax, code); // rMax
     val(Form1.nStartEdit.Text, n_Start, code); // N_Start
     val(Form1.betaEdit.Text, beta, code); // beta
     val(Form1.mortalityEdit.Text, mortality, code); // mortality
     val(Form1.mutationRateEdit.Text, mutationRate, code); // mutation rate
     val(Form1.mutationStrengthEdit.Text, mutationStrength, code); // mutation strength
     val(Form1.maxSeedsEdit.Text, MAXSEEDS, code); // max seeds
     val(Form1.disturbanceEdit.Text, p_disturbance, code); // expected time until next disturbance
     val(Form1.dispersalEdit.Text, dispersalRange, code); // maximum range of dispersal
     Form1.numbersChart.AxisList.BottomAxis.Range.Max := tMax;
     Form1.numbersChart.AxisList.LeftAxis.Range.Max := MAXSEEDS;


     { Replication Loop }

     for r := 1 to rMax do
     begin

          // print current replication number to 'debug' and 'rCurrentEdit'
          Form1.rCurrentEdit.Text := r.ToString();
          Form1.debug.Lines.Add('Replication #' + r.toString());

          // randomize ALL THE THINGS
          Randomize;

          // assign and read a random disturbance landscape file (landscape1.txt - landscape50.txt)
          if constantLandscape_checked then
          begin

               inputfilename := 'landscapes/landscape' + (random(50) + 1).toString() + '.txt';
               AssignFile(inputfile, inputfilename);
               Form1.debug.Lines.Add(inputfilename); // print filename to 'debug'
               Reset(inputfile); // reset cursor

          end;

          // initialize landscape (x/y loop)
          for x := 1 to XMAX do for y := 1 to YMAX do
          begin

               InitializeLandscape(x,y); // initialize empty patches (call procedure)
               if disturbance_checked and constantLandscape_checked then ReadLandscape(x,y); // is 'Disturbances' checked? is this patch disturbed? (call procedure)

          end;

          // initialize n_start plants at random coordinates (x/y loop)
          for i := 1 to n_Start do InitializePopulation();

          // clear canvas (for drawing the landscape)
          Form1.Canvas.Pen.Style := psClear;

          // clear data charts
          Form1.numbersSeries.Clear;

          // if 'Draw' is checked then draw on canvas (x/y loop)
          if draw_checked then for x := 1 to XMAX do for y := 1 to YMAX do Draw(x,y);


          { Time Step Loop }

          for t := 1 to tMax do
          begin

               // initialize patches per time step
               for x := 1 to XMAX do for y := 1 to YMAX do
               begin

                    inc(landscape[x,y].t_last_disturbance); // increase t_last_disturbance
                    landscape[x,y].competitorsAmount := 0; // reset competing seeds per patch to zero

               end;

               // if 'Disturbances' is checked then check for disturbance
               if disturbance_checked and (random() < p_disturbance) then
               begin

                    // if the program should vary landscape files then assign and read a new landscape file
                    if not constantLandscape_checked then
                    begin

                         inputfilename := 'landscapes/landscape' + (random(50) + 1).toString() + '.txt';
                         AssignFile(inputfile, inputfilename);
                         Form1.debug.Lines.Add(inputfilename); // print filename to 'debug'
                         Reset(inputfile); // reset cursor
                         for x := 1 to XMAX do for y := 1 to YMAX do ReadLandscape(x,y);

                    end;

                    for x := 1 to XMAX do for y := 1 to YMAX do Disturbance(x,y); // check if this patch is disturbed (x/y loop, call procedure)

               end;

               // death (x/y loop)
               for x := 1 to XMAX do for y := 1 to YMAX do Death(x,y);

               // reproduction (x/y loop)
               for x := 1 to XMAX do for y := 1 to YMAX do Reproduction(x,y);

               // dispersal (x/y loop)
               for x := 1 to XMAX do for y := 1 to YMAX do Dispersal(x,y);

               // growth (x/y loop)
               for x := 1 to XMAX do for y := 1 to YMAX do Growth(x,y);

               // draw on canvas (x/y loop)
               if draw_checked then for x := 1 to XMAX do for y := 1 to YMAX do Draw(x,y);

               // reset global data
               totalSeedAmount := 0;
               meanSeedAmount := 0;
               occupiedAmount := 0;

               // count occupied patches and analyze traits (expected number of seeds produced)
               for x := 1 to XMAX do for y := 1 to YMAX do if landscape[x,y].occupied then
               begin

                    totalSeedAmount := totalSeedAmount + landscape[x,y].seedAmountExpected;
                    inc(occupiedAmount);

               end;

               // calculate meanSeedAmount
               if occupiedAmount > 0 then
               begin

                    meanSeedAmount := totalSeedAmount / occupiedAmount;
                    Form1.numbersSeries.ADDXY(t,meanSeedAmount); // draw 'numbersSeries'

               end;

               // process messages and wait for tDel miliseconds before repeating the time step loop
               Application.ProcessMessages;
               Sleep(tDel);

               // write global data if 'Save Global Data' is checked and more than 90 % of simulation time has passed
               if Form1.saveCheckBox.Checked then
               if (t > (0.9 * tMax)) and (t mod 10 = 0) then writeln(outputfile, r:2, ',', t:4, ',', mortality:3:2, ',', beta:3:2, ',', MAXSEEDS:3, ',', dispersalRange:3, ',', p_disturbance:1:3, ',', meanSeedAmount:3:3, ',', constantLandscape_checked);

          end; // end of time step loop

          //write patch specific data if 'Save Patch Data' is checked and t = TMAX
          if Form1.patchSaveCheckBox.checked then
          begin

               AssignFile(patchoutputfile, patchfilename);
               Append(patchoutputfile);
               for x := 1 to XMAX do for y := 1 to YMAX do writeln(patchoutputfile, r:2, ',', mortality:3:2, ',', dispersalRange:3, ',', p_disturbance:1:3, ',', constantLandscape_checked, ',', x:4, ',', y:4, ',', landscape[x,y].occupied, ',', landscape[x,y].t_last_disturbance, ',', landscape[x,y].number_disturbances, ',',  landscape[x,y].seedAmountExpected);
               CloseFile(patchoutputfile);

          end;

     end; // end of replication loop

     // if 'Save Global Data' is checked then close the file
     if Form1.saveCheckBox.Checked then CloseFile(outputfile);

end;


{ Buttons }

////////////////
// RUN BUTTON //
////////////////
procedure TForm1.runButtonClick(Sender: TObject);

// pretty self-explanatory

begin

     Simulation();

end;

/////////////////
// EXIT BUTTON //
/////////////////
procedure TForm1.exitButtonClick(Sender: TObject);

// when the 'Exit' button is clicked on, the program is stopped

begin

     halt;

end;


{ Check Boxes }

////////////////////
// SAVE CHECK BOX //
////////////////////
procedure TForm1.saveCheckBoxChange(Sender: TObject);

// when the 'Save Global Data' check box is checked, global data will be saved to
// a textfile at location 'filename' (see procedure Simulation)

begin

     if saveCheckBox.Checked then
        if SaveDialog.Execute then
           filename := SaveDialog.FileName;
end;

//////////////////////////
// PATCH SAVE CHECK BOX //
//////////////////////////
procedure TForm1.patchSaveCheckBoxChange(Sender: TObject);

// when the 'Save Patch Data' check box is checked, patch-specific data will be
// saved to a textfile at location 'patchfilename' at the end of any replicaiton
// (see procedure simulation)

begin

     if patchSaveCheckBox.Checked then
        if patchSaveDialog.Execute then
           patchfilename := patchSaveDialog.FileName;

end;

///////////////////////////
// DISTURBANCE CHECK BOX //
///////////////////////////
procedure TForm1.disturbanceCheckBoxChange(Sender: TObject);

// when the 'Disturbances' check box is checked, some patches will frequently
// die due to disturbances (see procedure Disturbances)

begin

     disturbance_checked := Form1.disturbanceCheckBox.Checked;

end;

////////////////////
// DRAW CHECK BOX //
////////////////////
procedure TForm1.drawCheckBoxChange(Sender: TObject);

// when the 'Draw Landscape' check box is checked, the simulated landscape will
// be drawn in real-time (see procedure Draw)

begin

     draw_checked := Form1.drawCheckBox.Checked;

end;

/////////////////////////
// LANDSCAPE CHECK BOX //
/////////////////////////
procedure TForm1.landscapeCheckBoxChange(Sender: TObject);

// When the 'Constant Disturbance Landscape' check box is checked, the program
// will read a random landscape file in the beginning of every replication instead of
// reading a random landscape file whenever a disturbance is happening.

begin

     constantLandscape_checked := Form1.landscapeCheckBox.Checked;

end;


{ Input Fields }

///////////////////////////
// READ SIMULATION DELAY //
///////////////////////////
procedure TForm1.tDelEditChange(Sender: TObject);

// this has to be in a separate procedure which is controlled by the EditChange
// event so that the user can change the simulation speed at any time

var
  code : integer; // debugging index number

begin

     val(Form1.tDelEdit.Text, tDel, code);

end;

end.

