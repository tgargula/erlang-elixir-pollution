# Porównanie Enum _vs._ Stream

Wykonano po 5 pomiarów przy użyciu:
###### add_stations
```elixir
fn -> PollutionData.import_lines_from_csv("pollution.csv") |> PollutionData.add_stations end |> :timer.tc |> elem(0)
```
###### add_measurements
```elixir
fn -> PollutionData.import_lines_from_csv("pollution.csv") |> PollutionData.add_measurements end |> :timer.tc |> elem(0)
```

Z otrzymanych wyników obliczyłem średnią arytmetyczną i zamieściłem w tabeli poniżej.

|                  | Enum       | Stream     |
|------------------|------------|------------|
| add_stations     | 0,0275774s | 0,0347608s |
| add_measurements | 0,0449842s | 0,0506768s |

Można zauważyć, że wersja ze streamami działa nieco dłużej, mimo tego, że operacje na strumieniach
są z reguły szybsze. Jest to spowodowane tym, że konwersja ze streamów na obiekty enum jest kosztowna.
Strumieni powinno się więc używać wtedy, gdy wykonujemy dużo operacji.
