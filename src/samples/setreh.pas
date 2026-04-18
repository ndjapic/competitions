program setreh;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords} 
uses
    gset;
type
    Pint = ^int32;
    TLess = record
        class function c(L, R: Pint): Boolean; static;
    end;
    TintSet = specialize TSet<Pint, TLess>;
var
    s: TintSet;

class function TLess.c(L, R: Pint): Boolean;       // Our sorting function
begin
    Result := L^ < R^;
end;              

procedure AddItemToSet(x: integer);
var
    p: Pint;
begin
    new(p);
    p^ := x;
    s.Insert(p);
end;

procedure TestTSet();
var
    it : TintSet.TIterator;
    pMin: TintSet.PNode;
begin
    s := TintSet.Create;
    AddItemToSet(1);
    AddItemToSet(4);
    AddItemToSet(16);
    AddItemToSet(64);
    AddItemToSet(256);
    AddItemToSet(1024);
    AddItemToSet(4096);
    AddItemToSet(16384);
    AddItemToSet(65536);
    AddItemToSet(1); // Note, a duplicate !
    AddItemToSet(5);
    AddItemToSet(25);
    AddItemToSet(125);
    AddItemToSet(625);
    AddItemToSet(3125);
    AddItemToSet(15625);
    AddItemToSet(78125);
    
    it := s.Min;                  
    if it <> nil then begin
        pMin := it.FNode;             // So we can come back to it.
        repeat
            write(it.GetData^, ' ');
        until not it.Next;
        writeln;
        
        // OK, here we Free or Dispose as appropriate.
        it.FNode := pMin;
        repeat
            dispose(it.GetData);
        until not it.Next;
    end;
    it.Free;
    s.Free;
end;                                   

begin
    TestTSet();
end.
