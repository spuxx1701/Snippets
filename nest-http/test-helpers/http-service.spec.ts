/* eslint-disable @typescript-eslint/no-unused-vars */
import { Injectable } from '@nestjs/common';
import { AxiosResponse } from 'axios';
import { RequestOptions } from 'https';
import { HttpService } from 'src/http/services/http.service';

/**
 * This is a mock of the HttpService that will be included into the TestContainer
 * for the purpose of mocking outgoing HTTP requests. You need mock its functions
 * before triggering any HTTP requests in your tests, or else you will receive
 * an error.
 */
@Injectable()
export class MockHttpService extends HttpService {
  get = jest.fn(
    (
      url: string,
      options?: RequestOptions,
    ): Promise<Partial<AxiosResponse>> => {
      throw new Error(`You need to mock me! For example, do:
        container.httpService.get.mockImplementationOnce(async () => {
          return { data: 'Hello World!' };
        });`);
    },
  );
}
