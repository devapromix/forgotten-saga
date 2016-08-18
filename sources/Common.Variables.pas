unit Common.Variables;

interface

uses
  ForgottenSaga.Game;

const
  FSVersion = '0.0.3';              // Версия
  Copyright = 'Copyright (C) 2016 by Sergiy Tkach (DevApromix)';

var
  PanelWidth: Byte = 40;            // Ширина панели

const
  RaceName: array [TRaceEnum] of string =
    ('Avgu,Leo,Tan,Sho,Penr,Lok,Gron,Lar,Midr|sin,neg,zar,kar,tun,rel,bal,rin,kon|or,fin,shog,tal,rod,pin,ol,kan,on',
    'Had,Rod,Shag,Dor,Lid,Tar,Kreg,Bron,Shung|Garum,Turum,Ur,Utak,Udoom,Ud,Urak,Doon,Vuug|Kat,Shak,Gir,Bood,Dreg,Din,Grok,Rig,Sadr',
    'Blind,Glad,Proud,Sharp-sighted,Powerful,Dancer,Guarding,Thunderous,Night|Wolfhound,Wood-goblin,Destroyer,Crusher,Pathfinder,Astrologer,Bootes,Caretaker,Befouler');

implementation

end.
