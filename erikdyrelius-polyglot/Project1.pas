program Project1;
uses crt;

type
  GrowingVector = Record
    data: Array of Integer;
    size: Integer;
    dimension: Integer;
  end;

Procedure init(var gv: GrowingVector);
begin
  gv.dimension := 16;
  gv.size := 0;
  setLength(gv.data, gv.dimension);
end;

Procedure add(var gv: GrowingVector; value: Integer);
begin
  if gv.size >= gv.dimension then
  begin
    while gv.size >= gv.dimension do
    begin
      gv.dimension := gv.dimension * 2;
    end;
    setLength(gv.data, gv.dimension);
  end;
  gv.data[gv.size] := value;
  inc(gv.size);
end;

Function at(gv: GrowingVector; idx:Integer) : Integer;
begin
     at := gv.data[idx];
end;

Function length(gv: GrowingVector) : Integer;
begin
     length := gv.size;
end;

type BSTNode = Record
  value: Integer;
  left: ^BSTNode;
  right: ^BSTNode;
  end;

type IntSet = ^BSTNode;

Procedure init(var node: BSTNode; value: Integer);
begin
  node.value := value;
  node.left := nil;
  node.right := nil;
end;

Procedure init(var i: IntSet);
begin
     i := nil;
end;

Procedure add(var i:IntSet; value: Integer);
begin
  if i = nil then
  begin
    i := new(IntSet);
    init(i^, value);
  end
  else
  begin
    if value < i^.value then
    begin
      add(i^.left, value);
    end
    else if value > i^.value then
    begin
      add(i^.right, value);
    end;
  end;
end;

Function check(i:IntSet; value: Integer): Boolean;
begin
  if i = nil then
     check := false
  else
  begin
    if value = i^.value then
       check := true
    else if value < i^.value then
       check := check(i^.left, value)
    else
        check := check(i^.right, value);
  end;
end;

Procedure readFile(fileName:string; var gv:GrowingVector);
var
  theFile: text;
  i: Integer;
begin
  assign(theFile, fileName);
  reset(theFile);
  while not EOF(theFile) do
  begin
    ReadLn(theFile, i);
    add(gv, i);
  end;
  close(theFile);
end;

Function sumOfInts(gv:GrowingVector):Integer;
var
  idx, sum: Integer;
begin
  sum := 0;
  for idx := 0 to length(gv) do
  begin
    sum := sum + at(gv, idx);
  end;
  sumOfInts:=sum;
end;

Function findRepeat(gv:GrowingVector):Integer;
var
  idx, sum: Integer;
  found: Boolean;
  i: IntSet;
begin
  sum := 0;
  found := false;
  init(i);
  repeat
    for idx := 0 to length(gv)-1 do
    begin
      sum := sum + at(gv, idx);
      if check(i, sum) then
      begin
        found := true;
        findRepeat := sum;
        break;
      end
      else
        add(i, sum);
    end;
  until found;
end;

var
  gv: GrowingVector;

begin
  init(gv);
  readFile('day01.txt', gv);
  WriteLn('Solution to day 1, part 1:', sumOfInts(gv));
  WriteLn('Solution to day 1, part 2:', findRepeat(gv));
  readkey;
end.

