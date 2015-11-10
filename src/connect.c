#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <string.h>
#include <arpa/inet.h>

#include "connect.h"

/* int make_socket () */
/* { */
/* 	int sock; */
/* 	struct sockaddr_in name; */

/* 	/1* Create the socket. *1/ */
/* 	sock = socket (PF_INET, SOCK_STREAM, 0); */
/* 	if (sock < 0) */
/* 	{ */
/* 		/1* perror ("socket"); *1/ */
/* 		fprintf(stderr, "Error creating socket (%s)\n", strerror(errno)); */ 
/* 		return errno; */
/* 	} */

/* 	/1* Give the socket a name. *1/ */
/* 	name.sin_family = AF_INET; */
/* 	name.sin_port = htons (0); */
/* 	name.sin_addr.s_addr = htonl (INADDR_ANY); */
/* 	if (bind (sock, (struct sockaddr *) &name, sizeof (name)) < 0) */
/* 	{ */
/* 		/1* perror ("bind"); *1/ */
/* 		fprintf(stderr, "Error binding (%s)\n", strerror(errno)); */ 
/* 		return errno; */
/* 		/1* exit (EXIT_FAILURE); *1/ */
/* 	} */

/* 	return sock; */
/* } */

int connect_tcp(int sockfd,uint32_t ip,uint16_t port)
{
	int res;
	struct sockaddr_in addr; 
	long arg; 
	fd_set myset; 
	struct timeval tv; 
	int valopt; 
	socklen_t lon; 

	addr.sin_family = AF_INET; 
	addr.sin_port = htons(port); 
	addr.sin_addr.s_addr = ip; 

	// Set non-blocking 
	if( (arg = fcntl(sockfd, F_GETFL, NULL)) < 0) { 
		fprintf(stderr, "Error fcntl(..., F_GETFL) (%s)\n", strerror(errno)); 
		return errno;
	} 
	arg |= O_NONBLOCK; 
	if( fcntl(sockfd, F_SETFL, arg) < 0) { 
		fprintf(stderr, "Error fcntl(..., F_SETFL) (%s)\n", strerror(errno)); 
		return errno;
	} 
	// Trying to connect with timeout 
	res = connect(sockfd, (struct sockaddr *)&addr, sizeof(addr)); 
	if (res < 0) { 
		if (errno == EINPROGRESS) { 
			fprintf(stderr, "EINPROGRESS in connect() - selecting\n");  
			do {
			   tv.tv_sec = 10; 
			   tv.tv_usec = 0; 
			   FD_ZERO(&myset); 
			   FD_SET(sockfd, &myset); 
			   /* res = 1; */
			   res = select(sockfd+1, NULL, &myset, NULL, &tv); 
			   if (res < 0 && errno != EINTR) { 
				fprintf(stderr, "Error connecting %d - %s\n", errno, strerror(errno)); 
				return errno;
				/* exit(0); */ 
			   } 
			   else if (res > 0) { 
			      // Socket selected for write 
			      lon = sizeof(int); 
			      if (getsockopt(sockfd, SOL_SOCKET, SO_ERROR, (void*)(&valopt), &lon) < 0) { 

				 fprintf(stderr, "Error in getsockopt() %d - %s\n", errno, strerror(errno)); 
				 /* exit(0); */ 
				 return errno;
			      } 
			      // Check the value returned... 
			      if (valopt) { 
				 fprintf(stderr, "Error in delayed connection() %d - %s\n", valopt, strerror(valopt)); 
				 /* exit(0); */ 
				 return valopt;
			      } 
			      break; 
			      /* return 0; */
			   } 
			   else { 
			      fprintf(stderr, "Timeout in select() - Cancelling!\n"); 
			      /* exit(0); */ 
			      return -1;
			   } 
			} while (1);
		} 
		else { 
			fprintf(stderr, "Error connecting %d - %s\n", errno, strerror(errno)); 
			return errno;
			/* exit(0); */ 
		} 
	} 
	// Set to blocking mode again... 
	if( (arg = fcntl(sockfd, F_GETFL, NULL)) < 0) { 
		fprintf(stderr, "Error fcntl(..., F_GETFL) (%s)\n", strerror(errno)); 
		return errno;
		/* exit(0); */ 
	} 
	arg &= (~O_NONBLOCK); 
	if( fcntl(sockfd, F_SETFL, arg) < 0) { 
		fprintf(stderr, "Error fcntl(..., F_SETFL) (%s)\n", strerror(errno)); 
		return errno;
		/* exit(0); */ 
	} 
  	return 0;
}

/* int main() */
/* { */
/* 	int sockfd; */
/* 	sockfd = make_socket(); */
/* 	/1* printf("Inet %u\n",inet_addr("220.100.173.50")); *1/ */
/* 	return connect_tcp(sockfd,inet_addr("172.27.22.223"),47350); */	
/* } */
