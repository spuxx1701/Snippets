import {
  DynamicModule,
  ForwardReference,
  LoggerService,
  Provider,
  Type,
} from '@nestjs/common';
import { Test, TestingModule } from '@nestjs/testing';
import { TypeOrmModule } from '@nestjs/typeorm';
import { EntityClassOrSchema } from '@nestjs/typeorm/dist/interfaces/entity-class-or-schema.type';
import { DataSource } from 'typeorm';
import { MockHttpService } from './http-service-spec';
import { HttpService } from 'src/http/services/http.service';
import { HttpService as NestHttpService } from '@nestjs/axios';

/**
 * The TestContainer provides a simualted Nest.js application for testing purposes. It essentially wraps
 * Nest's Test.createTestContainer(), but offers a customized API for easier handling and access.
 * @param module The testing module.
 * @param dataSource (optional) The TypeOrm DataSource. Will be undefined if no TypeOrmModule was included into the container.
 * @param httpService (optional) The mocked HttpService. Will be undefined if 'mockHttp' was set to false.
 */
export class TestContainer {
  module: TestingModule;
  dataSource?: DataSource;
  httpService?: MockHttpService;

  constructor(init: TestContainer) {
    Object.assign(this, init);
  }
}

/**
 * Creates a TestContainer that allows integrated testing of components.
 * @param options.imports (optional) A list of imports you need for testing.
 * @param options.controllers (optional) A list of controllers you need for testing.
 * @param options.providers (optional) A list of providers you need for testing.
 * @param options.entities (optional) A list of entities you need for testing. If provided, TypeOrmModule with an in-memory DB will be included into the container
 * @param options.dataSourceName (optional) When providing entities that use a names data source, you might also need to provide the data source name.
 * @param options.logger (optional) A custom logger can be provided. Helpful when you want to spy on logging functionality.
 * @param options.mockHttp (optional) Whether HttpService should be mocked. If set to 'true', a mocked version of HttpService will be included into the container.
 * @param options.disableAuth (optional) Whether authentication should be disabled in the entire container.
 * @returns The test container.
 */
export const createTestContainer = async (options: {
  imports?: (
    | Type<any>
    | DynamicModule
    | Promise<DynamicModule>
    | ForwardReference<any>
  )[];
  controllers?: Type<any>[];
  providers?: Provider<any>[];
  entities?: EntityClassOrSchema[];
  dataSourceName?: string;
  logger?: LoggerService;
  mockHttp?: boolean;
  disableAuth?: boolean;
}): Promise<TestContainer> => {
  const imports = [...(options.imports || [])];
  const controllers = [...(options.controllers || [])];
  const providers = [...(options.providers || [])];

  // Set APP_ENVIRONMENT to 'test' for this node process
  process.env.APP_ENVIRONMENT = 'test';

  // Disable authentcation if requested
  if (options?.disableAuth) {
    process.env.AUTH_ENABLE = 'false';
  }

  // If entities were provided, we'll need to create a TypeOrm module
  // for testing purposes that uses an in-memory sqlite3 database
  let dataSource: DataSource | undefined;
  if (options.entities && options.entities.length > 0) {
    dataSource = new DataSource({
      name: options.dataSourceName || 'default',
      type: 'better-sqlite3',
      database: ':memory:',
      dropSchema: true,
      synchronize: true,
      entities: options.entities,
    });
    imports.push(
      ...createOrmModule(dataSource, options.entities, options.dataSourceName),
    );
  }

  // If mockHttp was provided and set to true, we'll need to include a mocked HttpService.
  // When doing so, we will also override Nest's internal HttpService with an empty value
  // or else the mocked HttpService's dependencies won't resolve.
  if (options.mockHttp) {
    providers.push({ provide: NestHttpService, useValue: {} });
    providers.push({ provide: HttpService, useClass: MockHttpService });
  }

  // Create the precompiled version of the module
  let builder = Test.createTestingModule({
    imports,
    controllers,
    providers,
  });

  // If a custom logger has been provided, we'll override the module's logger
  if (options.logger) {
    builder = builder.setLogger(options.logger);
  }

  // Compile the module
  const module = await builder.compile();

  // If mockHttp was provided and set to true, we will retrieve the mocked httpService
  // from the module and add it to the container so it can be accessed easily when
  // writing tests.
  let httpService: MockHttpService | undefined;
  if (options.mockHttp) {
    httpService = module.get<MockHttpService>(HttpService);
  }

  return new TestContainer({ module, dataSource, httpService });
};

/**
 * Creates and returns a TypeOrm module for testing purposes that uses an
 * in-memory sqlite3 database. Is best not used directly, but via the TestContainer.
 * @param dataSource The data source.
 * @param entities The entities that should be used.
 * @returns The TypeOrm module.
 */
const createOrmModule = (
  dataSource: DataSource,
  entities: EntityClassOrSchema[],
  dataSourceName?: string,
) => [
  TypeOrmModule.forRootAsync({
    name: dataSourceName || 'default',
    useFactory: () => ({
      type: 'better-sqlite3',
      database: ':memory:',
      entities,
      dropSchema: true,
      synchronize: true,
    }),
    dataSourceFactory: async () => dataSource.initialize(),
  }),
  TypeOrmModule.forFeature(entities, dataSource),
];
