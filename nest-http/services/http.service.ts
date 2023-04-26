import { HttpService as NestHttpService } from '@nestjs/axios';
import { Injectable } from '@nestjs/common';
import { AxiosError, AxiosResponse } from 'axios';
import { catchError, firstValueFrom } from 'rxjs';

export interface RequestOptions {
  headers?: object;
}

@Injectable()
export class HttpService {
  constructor(private readonly httpService: NestHttpService) {}

  /**
   * Triggers an outgoing GET request.
   * @param url The url.
   * @param options The request options.
   * @returns The response object.
   */
  async get(
    url: string,
    options?: RequestOptions,
  ): Promise<Partial<AxiosResponse>> {
    return firstValueFrom(
      this.httpService
        .get(url, {
          headers: {
            ...options?.headers,
          },
        })
        .pipe(
          catchError((error: AxiosError) => {
            throw error;
          }),
        ),
    );
  }
}
