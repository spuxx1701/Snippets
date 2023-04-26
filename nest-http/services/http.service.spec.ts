import {
  HttpModule as NestHttpModule,
  HttpService as NestHttpService,
} from '@nestjs/axios';
import { HttpService } from './http.service';
import { createTestContainer } from 'test/helpers/test-container-spec';
import { of } from 'rxjs';

describe('Http | HttpService', () => {
  let service: HttpService;
  let nestHttpService: NestHttpService;

  beforeEach(async () => {
    const { module } = await createTestContainer({
      imports: [NestHttpModule.register({ timeout: 1000 })],
      providers: [HttpService],
    });
    service = module.get<HttpService>(HttpService);
    nestHttpService = module.get<NestHttpService>(NestHttpService);
  });

  describe('get', () => {
    it("should trigger the expected GET request'", async () => {
      const url = 'https://hello.world';
      const result = { data: 'hello world' } as any;
      const getSpy = jest.spyOn(nestHttpService, 'get');
      getSpy.mockImplementationOnce(() => of(result));

      const actual = await service.get(url);

      expect(getSpy).toHaveBeenCalledTimes(1);
      expect(getSpy).toHaveBeenCalledWith(url, {
        headers: { Cookie: undefined },
      });
      expect(actual).toStrictEqual(result);
    });

    it('should raise an error', async () => {
      const getSpy = jest.spyOn(nestHttpService, 'get');
      getSpy.mockImplementationOnce(() => {
        throw new Error('Something bad happened!');
      });
      await expect(service.get('some invalid url')).rejects.toThrow();
    });
  });
});
