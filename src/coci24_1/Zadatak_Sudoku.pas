program Zadatak_Sudoku;
var
    i, j: int8;
    ch: char;
    ans: boolean;
    s: array [1 .. 9, 1 .. 9] of char;
    c: array ['1' .. '9'] of int8;

procedure read3ch(i, j: int8);
begin
    read(s[i, j]);
    read(s[i, j-2]);
    read(s[i, j-1]);
    read(s[i, j]);
end;

procedure read1ln(i: int8);
begin
    read3ch(i, 3);
    read3ch(i, 6);
    read3ch(i, 9);
    readln(ch);
end;

procedure read3ln(i: int8);
begin
    readln;
    read1ln(i-2);
    read1ln(i-1);
    read1ln(i);
end;

function check_rectangle(i1, i2, j1, j2: int8): boolean;
var
    i, j: int8;
    ans: boolean;
begin
    for ch := '1' to '9' do c[ch] := 0;
    ans := true;
    for i := i1 to i2 do
        for j := j1 to j2 do begin
            ch := s[i, j];
            if ch <> '.' then inc(c[ch]);
            ans := ans and (c[ch] < 2);
        end;
    check_rectangle := ans;
end;

begin

    read3ln(3);
    read3ln(6);
    read3ln(9);
    readln;

    ans := true;
    for i := 1 to 9 do ans := ans and check_rectangle(i, i, 1, 9);
    for j := 1 to 9 do ans := ans and check_rectangle(1, 9, j, j);

    for i := 1 to 3 do
        for j := 1 to 3 do
            ans := ans and check_rectangle(3*i-2, 3*i, 3*j-2, 3*j);

    if ans then
        writeln('OK')
    else
        writeln('GRESKA');

end.
