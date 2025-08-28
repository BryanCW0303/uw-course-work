2025Spring STAT534 
Final Project
Bryan Ng, 2427348

This project implements a simulation of a volleyball game using MPI in C++. The simulation models a best‐of‐five‐set volleyball game between two teams of six players each, with a referee coordinating play. Each MPI process represents either the referee (process 0) or one of the twelve players (processes 1–12). The ball is passed between players or returned to the referee as messages, and points are scored according to the rules described below. A detailed log of every action, score update, and set outcome is written to a log file volleyball_log.txt.

Commands on the Cluster:

srun --pty --ntasks=13 --time=60 --mem-per-cpu=1000 --partition=student /bin/bash
module OpenMPI
module load gompi/2023b
mpic++ project.cpp -o project
mpirun -np 13 ./project