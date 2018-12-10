%%
%% SETools - SysVision Erlang Tools
%% 
%% Copyright (C) 2017-18 SysVision - Consultadoria e Desenvolvimento em Sistemas de Informática, Lda.  
%% 
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%%
-module(set_ga_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run_ordered_chromosome_ga/7, run_ordered_chromosome_ga/8]).

run_ordered_chromosome_ga(Genes, FitnessFunction, PopulationSize, StagnationRuns, ElitismRatio, TournmentSize, MutationProbability) ->
	Population = generate_population(PopulationSize, Genes, FitnessFunction),
	run_ordered_chromosome_ga(FitnessFunction, Population, StagnationRuns, ElitismRatio, TournmentSize, MutationProbability).

run_ordered_chromosome_ga(Genes, FitnessFunction, PopulationSize, InitialPopulationFunction, StagnationRuns, ElitismRatio, TournmentSize,
                          MutationProbability) ->
	Population = InitialPopulationFunction(PopulationSize, Genes, FitnessFunction),
	run_ordered_chromosome_ga(FitnessFunction, Population, StagnationRuns, ElitismRatio, TournmentSize, MutationProbability).

%% ====================================================================
%% Internal functions
%% ====================================================================
run_ordered_chromosome_ga(FitnessFunction, Population, StagnationRuns, ElitismRatio, TournmentSize, MutationProbability) ->
	{Fitness, Solution} = converge(Population, StagnationRuns, ElitismRatio, TournmentSize, MutationProbability, FitnessFunction),
	{ok, Solution, Fitness}.

generate_population(Size, Genes, FitnessFunc) when Size >= 0 ->
	generate_population(Size, Genes, FitnessFunc, []).

generate_population(0, _Genes, _FitnessFunc, Population) ->
	Population;
generate_population(Size, Genes, FitnessFunc, Population) ->
	Chromosome = shuffle(Genes),
	Fitness = FitnessFunc(Chromosome),
	generate_population(Size - 1, Genes, FitnessFunc, [{Fitness, Chromosome}|Population]).

converge(Population, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc) ->
	converge(Population, 1, undefined, undefined, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc).

converge(Population, StagnationRuns, _LastAverage, _LastBest, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc) ->
	NewPopulation = iterate(Population, elitism(Population, ElitismRatio), TournmentK, MutationProb, FitnessFunc),
	Average = calculate_fitness_average(NewPopulation),
	{Best, _} = get_most_fit(NewPopulation),
	converge(NewPopulation, StagnationRuns + 1, Average, Best, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc);
converge(Population, Run, LastAverage, LastBest, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc)
  when Run rem StagnationRuns =:= 0 ->
	NewPopulation = iterate(Population, elitism(Population, ElitismRatio), TournmentK, MutationProb, FitnessFunc),
	Average = calculate_fitness_average(NewPopulation),
	{Best, Chromosome} = get_most_fit(NewPopulation),
	if
		Average =< LastAverage andalso Best =:= LastBest ->
			{Best, Chromosome};
		true ->
			converge(NewPopulation, Run + 1, Average, Best, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc)
	end;
converge(Population, Run, LastAverage, LastBest, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc) ->
	NewPopulation = iterate(Population, elitism(Population, ElitismRatio), TournmentK, MutationProb, FitnessFunc),
	converge(NewPopulation, Run + 1, LastAverage, LastBest, StagnationRuns, ElitismRatio, TournmentK, MutationProb, FitnessFunc).

iterate(OldPopulation, NewPopulation, _TournmentK, _MutationProb, _FitnessFunc) when length(NewPopulation) >= length(OldPopulation) ->
	NewPopulation;
iterate(OldPopulation, NewPopulation, TournmentK, MutationProb, FitnessFunc) ->
	Parent1 = tournment(OldPopulation, TournmentK),
	Parent2 = tournment(OldPopulation, TournmentK),
	{Child1, Child2} = crossover(Parent1, Parent2, FitnessFunc),
	{MChild1, MChild2} =
		case rand:uniform() =< MutationProb of
			true ->
				{mutate(Child1, FitnessFunc), mutate(Child2, FitnessFunc)};
			false ->
				{Child1, Child2}
		end,
	iterate(OldPopulation, [MChild1,MChild2|NewPopulation], TournmentK, MutationProb, FitnessFunc).

select_fittest({Fitness1, Chromosome1}, {Fitness2, _Chromosome2}) when Fitness1 > Fitness2 ->
	{Fitness1, Chromosome1};
select_fittest(_Elem1, Elem2) ->
	Elem2.

tournment(Population, K) ->
	PopulationSize = length(Population),
	[First|Sample] = [lists:nth(rand:uniform(PopulationSize), Population) || _ <- lists:seq(1, K)],
	lists:foldr(fun select_fittest/2, First, Sample).

% Order Crossover Operator (ox1)
crossover({_Fitness1, Parent1}, {_Fitness2, Parent2}, FitnessFunc) ->
	Length = length(Parent1),
	{Idx1, Idx2} = generate_section_indexes(Length),
	Child1 = crossover(Parent1, Idx1, Idx2, Parent2),
	Child2 = crossover(Parent2, Idx1, Idx2, Parent1),
	Fitness1 = FitnessFunc(Child1),
	Fitness2 = FitnessFunc(Child2),
	{{Fitness1, Child1}, {Fitness2, Child2}}.

crossover(Parent1, Idx1, Idx2, Parent2) ->
	Mid = lists:sublist(Parent1, Idx1 + 1, Idx2 - Idx1),
	Remaining = Parent2 -- Mid,
	{Start, End} = lists:split(Idx1, Remaining),
	Start ++ Mid ++ End.

% Swap Mutation
mutate({_Fitness, Chromosome}, FitnessFunc) ->
	Length = length(Chromosome),
	{Idx1, Idx2} = generate_swap_indexes(Length),
	NewChromosome = swap(Chromosome, Idx1, Idx2),
	{FitnessFunc(NewChromosome), NewChromosome}.

generate_section_indexes(Length) ->
	X = rand:uniform(Length + 1) - 1,
	Y = rand:uniform(Length + 1) - 1,
	if
		X =< Y -> {X, Y};
		X > Y -> {Y, X}
	end.

generate_swap_indexes(Length) ->
	X = rand:uniform(Length),
	Y = rand:uniform(Length),
	if
		X =< Y -> {X, Y};
		X > Y -> {Y, X}
	end.

calculate_fitness_average(Population) ->
	Fitnesses = [Fitness || {Fitness, _} <- Population],
	lists:sum(Fitnesses) / length(Fitnesses).

get_most_fit(Population) ->
	lists:max(Population).

elitism(Population, Ratio) ->
	Size = round(length(Population) * Ratio),
	lists:sublist(lists:reverse(lists:sort(Population)), Size).

shuffle(List) ->
	[X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

swap(List, Idx, Idx) -> List;
swap(List, Idx1, Idx2) ->
	Elem1 = lists:nth(Idx1, List),
	Elem2 = lists:nth(Idx2, List),
	swap(List, Idx1, Idx2, Elem1, Elem2).

swap([], _Idx1, _Idx2, _Elem1, _Elem2) -> [];
swap(List, Idx1, Idx2, _Elem1, _Elem2) when Idx1 =< 0, Idx2 =< 0 -> List;
swap([_X|Xs], 1, Idx2, Elem1, Elem2) ->
	[Elem2|swap(Xs, 0, Idx2 - 1, Elem1, Elem2)];
swap([_X|Xs], Idx1, 1, Elem1, Elem2) ->
	[Elem1|swap(Xs, Idx1 - 1, 0, Elem1, Elem2)];
swap([X|Xs], Idx1, Idx2, Elem1, Elem2) ->
	[X|swap(Xs, Idx1 - 1, Idx2 - 1, Elem1, Elem2)].
