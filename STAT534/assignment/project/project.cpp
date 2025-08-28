#include <mpi.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <ctime>
#include <cstring>

// MPI message
#define TAG_SERVE  1   
#define TAG_PLAY   2   
#define TAG_POINT  3   
#define TAG_LOG    4   
#define TAG_END    5   

static const int LOG_MSG_MAX = 100;

// determine team from rank (0 = Team A, 1 = Team B)
inline int team_of(int rank) {
    if (rank >= 1 && rank <= 6) return 0;
    if (rank >= 7 && rank <= 12) return 1;
    return -1; // invalid for referee
}

// pick a random player rank from a given team (0 = A, 1 = B)
int random_player_from_team(int team) {
    if (team == 0) {
        return 1 + (std::rand() % 6);   // ranks 1..6
    } else {
        return 7 + (std::rand() % 6);   // ranks 7..12
    }
}

// pick a random teammate from same team excluding self
int random_teammate(int my_rank) {
    int tm;
    int my_team = team_of(my_rank);
    do {
        if (my_team == 0) {
            tm = 1 + (std::rand() % 6);
        } else {
            tm = 7 + (std::rand() % 6);
        }
    } while (tm == my_rank);
    return tm;
}

// format and send a log message (string) from a player to referee (rank 0)
void send_log(int my_rank, const std::string &msg) {
    char buffer[LOG_MSG_MAX];
    std::snprintf(buffer, LOG_MSG_MAX, "Player %2d: %s", my_rank, msg.c_str());
    MPI_Send(buffer, LOG_MSG_MAX, MPI_CHAR, 0, TAG_LOG, MPI_COMM_WORLD);
}

int main(int argc, char **argv) {
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    // seed random generator differently per process
    std::srand(static_cast<unsigned int>(std::time(nullptr)) + rank * 100);

    if (rank == 0) {
        // referee part  

        std::ofstream log_file("volleyball_log.txt");
        if (!log_file.is_open()) {
            std::cerr << "Referee: Failed to open log file.\n";
            MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
        }

        // initialize match state
        int sets_won_A = 0;
        int sets_won_B = 0;

        // overall match loop (best of 5 sets)
        while (sets_won_A < 3 && sets_won_B < 3) {
            int current_set = sets_won_A + sets_won_B + 1;
            int set_points_A = 0;
            int set_points_B = 0;

            // determine points needed to win this set
            int to_win = (current_set < 5) ? 25 : 15;

            // log: start of new set
            log_file << "===== GAME START! " << current_set << " =====\n";
            log_file << "Score (Sets - A:B): " << sets_won_A << ":" << sets_won_B << "\n";
            log_file.flush();

            // decide initial serving team and player
            int serving_team = ( (std::rand() / static_cast<double>(RAND_MAX)) < 0.5 ? 0 : 1 );
            int next_server = random_player_from_team(serving_team);
            {
                std::ostringstream oss;
                oss << "Referee: Initial serve to Player " << next_server 
                    << " (Team " << (serving_team == 0 ? "A" : "B") << ").";
                log_file << oss.str() << "\n";
                log_file.flush();
            }

            // send the first serve to the chosen server
            int dummy = 0;
            MPI_Send(&dummy, 1, MPI_INT, next_server, TAG_SERVE, MPI_COMM_WORLD);

            // handle each point until set is won
            bool set_over = false;
            while (!set_over) {
                MPI_Status status;
                // probe for any incoming message
                MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

                if (status.MPI_TAG == TAG_LOG) {
                    // receive a log message from a player and write it
                    char logmsg[LOG_MSG_MAX];
                    MPI_Recv(logmsg, LOG_MSG_MAX, MPI_CHAR, status.MPI_SOURCE, TAG_LOG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    log_file << logmsg << "\n";
                    log_file.flush();
                }
                else if (status.MPI_TAG == TAG_POINT) {
                    int buf[3];
                    MPI_Recv(buf, 3, MPI_INT, status.MPI_SOURCE, TAG_POINT, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    int code          = buf[0];  // 1=serve_out, 2=serve_point, 3=volley_score, 4=volley_fail
                    int last_player   = buf[1];  // rank of the player who generated this point event
                    int scoring_team  = buf[2];  // 0 = Team A scored, 1 = Team B scored

                    // update set score
                    if (scoring_team == 0) {
                        set_points_A++;
                    } else {
                        set_points_B++;
                    }

                    {
                        std::ostringstream oss;
                        oss << "Referee: Point scored by Team " << (scoring_team == 0 ? "A" : "B") 
                            << " -> Updated score in Set " << current_set << ": A " << set_points_A 
                            << " - B " << set_points_B;
                        log_file << oss.str() << "\n";
                        log_file.flush();
                    }

                    // check if set is over (must win by 2 points)
                    if ((set_points_A >= to_win || set_points_B >= to_win) &&
                        std::abs(set_points_A - set_points_B) >= 2) {
                        // set winner
                        int set_winner = (set_points_A > set_points_B ? 0 : 1);
                        if (set_winner == 0) {
                            sets_won_A++;
                        } else {
                            sets_won_B++;
                        }
                        {
                            std::ostringstream oss;
                            oss << "===== Set " << current_set << " winner: Team " 
                                << (set_winner == 0 ? "A" : "B") << " =====";
                            log_file << oss.str() << "\n";
                            log_file << "Sets Score: A " << sets_won_A << " - B " << sets_won_B << "\n\n";
                            log_file.flush();
                        }
                        set_over = true;
                    }
                    else {
                        // determine next server based on code
                        if (code == 2 || code == 3) {
                            // either serve_point (code=2) or volley_score (code=3): last_player serves next
                            next_server = last_player;
                        }
                        else {
                            // serve_out (1) or volley_fail (4): opposing team scored; pick random from scoring_team
                            next_server = random_player_from_team(scoring_team);
                        }
                        {
                            std::ostringstream oss;
                            oss << "Referee: Next serve goes to "  
                                << " Team " << (team_of(next_server)==0 ? "A" : "B") << " Player " << next_server;
                            log_file << oss.str() << "\n";
                            log_file.flush();
                        }
                        // Send next serve
                        MPI_Send(&dummy, 1, MPI_INT, next_server, TAG_SERVE, MPI_COMM_WORLD);
                    }
                }
            } 

            
        }
        // determine final winner
        int match_winner = (sets_won_A > sets_won_B) ? 0 : 1;
        log_file << "===== Match Winner: Team " << (match_winner == 0 ? "A" : "B") << " =====\n";
        log_file << "Final Sets: A " << sets_won_A << " - B " << sets_won_B << "\n";
        log_file << "===== End of Match =====\n";
        log_file.flush();

        // end the MPI process
        int dummy_end = 0;
        for (int p = 1; p <= 12; ++p) {
            MPI_Send(&dummy_end, 1, MPI_INT, p, TAG_END, MPI_COMM_WORLD);
        }

        log_file.close();
    }
    else {
        // player part  

        // identify the team of player
        int my_team = team_of(rank);     // 0 = Team A, 1 = Team B
        int opponent_team = 1 - my_team;

        bool continue_playing = true;
        while (continue_playing) {
            MPI_Status status;
            // wait for any message: TAG_SERVE, TAG_PLAY, or TAG_END
            int recv_buf;
            MPI_Recv(&recv_buf, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

            if (status.MPI_TAG == TAG_END) {
                continue_playing = false;
                break;
            }
            else if (status.MPI_TAG == TAG_SERVE) {
                // serve ball
                send_log(rank, "Serving the ball");

                double r = std::rand() / static_cast<double>(RAND_MAX);

                if (r < 0.25) {
                    // ball goes out of bounds, opponent scores
                    send_log(rank, "Serve out of bounds");
                    int code = 1; // serve_out
                    int buf[3] = { code, rank, opponent_team };
                    MPI_Send(buf, 3, MPI_INT, 0, TAG_POINT, MPI_COMM_WORLD);
                    // wait for next TAG_SERVE or TAG_END
                }
                else if (r < 0.25 + 0.50) {
                    // ball lands in court, my team scores and serve again
                    send_log(rank, "Serve for point for ball in court");
                    int code = 2; // serve_point
                    int buf[3] = { code, rank, my_team };
                    MPI_Send(buf, 3, MPI_INT, 0, TAG_POINT, MPI_COMM_WORLD);
                    // Wait for next TAG_SERVE
                }
                else {
                    // intercepted by one of the opponents
                    // choose a random opponent to receive the ball
                    int receiver = random_player_from_team(opponent_team);
                    send_log(rank, "Serve intercepted");
                    send_log(rank, (std::string("Intercepted by Player ") + std::to_string(receiver)).c_str());
                    int touches = 1;  // first touch by opponent
                    MPI_Send(&touches, 1, MPI_INT, receiver, TAG_PLAY, MPI_COMM_WORLD);
                }
            }
            else if (status.MPI_TAG == TAG_PLAY) {
                // received the ball during a rally
                int touches = recv_buf; // number of consecutive touches by my team so far
                std::ostringstream oss_recv;
                oss_recv << "Received ball with touches=" << touches;
                send_log(rank, oss_recv.str());

                if (touches < 3) {
                    double r = std::rand() / static_cast<double>(RAND_MAX);
                    if (r < 0.50) {
                        // successful pass to a teammate
                        int teammate = random_teammate(rank);
                        std::ostringstream oss;
                        oss << "Passing to teammate Player " << teammate;
                        send_log(rank, oss.str());

                        int next_touches = touches + 1;
                        MPI_Send(&next_touches, 1, MPI_INT, teammate, TAG_PLAY, MPI_COMM_WORLD);
                    }
                    else if (r < 0.50 + 0.25) {
                        // failed pass -> opponent scores
                        send_log(rank, "Pass failed -> opponent team scores");
                        int code = 4; // volley_fail
                        int buf[3] = { code, rank, opponent_team };
                        MPI_Send(buf, 3, MPI_INT, 0, TAG_POINT, MPI_COMM_WORLD);
                    }
                    else {
                        // successful volley score -> my team scores; the same player serve again
                        send_log(rank, "Volley score -> my team scores");
                        int code = 3; // volley_score
                        int buf[3] = { code, rank, my_team };
                        MPI_Send(buf, 3, MPI_INT, 0, TAG_POINT, MPI_COMM_WORLD);
                    }
                }
                else {
                    // touches >= 3: third touch by my team
                    double r = std::rand() / static_cast<double>(RAND_MAX);
                    if (r < 0.50) {
                        // my team scores
                        send_log(rank, "Third touch: volley score");
                        int code = 3; // volley_score
                        int buf[3] = { code, rank, my_team };
                        MPI_Send(buf, 3, MPI_INT, 0, TAG_POINT, MPI_COMM_WORLD);
                    }
                    else {
                        // opponent scores
                        send_log(rank, "Third touch: volley fail -> opponent scores");
                        int code = 4;
                        int buf[3] = { code, rank, opponent_team };
                        MPI_Send(buf, 3, MPI_INT, 0, TAG_POINT, MPI_COMM_WORLD);
                    }
                }
            }
        }
    }

    MPI_Finalize();
    return 0;
}
